{-# LANGUAGE TemplateHaskell #-}

module Test.Polysemy.Mock.TH where

import Data.Bifunctor (first)
import Data.List (foldl')
import Language.Haskell.TH hiding (Strict)
import Polysemy (interpret, pureT, reinterpretH)
import Polysemy.Internal (embed, send)
import Polysemy.Internal.TH.Common
import Polysemy.State (get, put)
import Test.Polysemy.Mock

genMock :: Name -> Q [Dec]
genMock effName = do
  (_, constructors) <- getEffectMetadata effName
  -- MockImpl
  let mockImplReturnType = foldl' AppT (ConT ''MockImpl) [ConT effName, VarT (mkName "m")]
  let mockImplDataType = mockImplReturnType `AppT` VarT (mkName "a")
  let mockImplConstructors =
        map (mkMockConstructor mockImplReturnType) constructors
          <> map (mkMockReturns mockImplReturnType) constructors
          <> map (mkMockCalls mockImplReturnType) constructors
  let mockImplD = DataInstD [] Nothing mockImplDataType Nothing mockImplConstructors []
  -- MockState
  let mockStateConName = mkName (nameBase ''MockState)
  let mockStateRec =
        map mkMockStateCallsField constructors
          <> map mkMockStateReturnsField constructors
  let mockStateConstructor = RecC mockStateConName mockStateRec
  let mockStateD = DataInstD [] Nothing (AppT (ConT ''MockState) (ConT effName)) Nothing [mockStateConstructor] []
  -- initialMockState
  let initialStateExps =
        map mkInitialCalls constructors
          <> map mkInitialReturns constructors
  let initialStateBody = NormalB (RecConE mockStateConName initialStateExps)
  let initialStateD = FunD 'initialMockState [Clause [] initialStateBody []]
  -- mock
  let mockMatches = map mkMockMatch constructors
  let mockBody = NormalB (AppE (VarE 'interpret) (LamCaseE mockMatches))
  let mockD = FunD 'mock [Clause [] mockBody []]
  -- mockToState
  let mockToStateMatches =
        map mkMockToStateMatch constructors
          <> map mkReturnsToStateMatch constructors
          <> map mkCallsToStateMatch constructors
  let mockToStateBody = NormalB (AppE (VarE 'reinterpretH) (LamCaseE mockToStateMatches))
  let mockToStateD = FunD 'mockToState [Clause [] mockToStateBody []]
  -- Bring it together
  pure
    [ InstanceD
        Nothing
        []
        (ConT ''Mock `AppT` ConT effName)
        [ mockImplD,
          mockStateD,
          initialStateD,
          mockD,
          mockToStateD
        ]
    ]

mkMockConstructor :: Type -> ConLiftInfo -> Con
mkMockConstructor t c =
  let args = (map (first (const defaultBang)) $ cliFunArgs c)
   in GadtC [mockConName c] args (AppT t $ cliEffRes c)

mkMockReturns :: Type -> ConLiftInfo -> Con
mkMockReturns t c =
  GadtC [returnsConName c] [(defaultBang, returnsFunctionType c)] (AppT t $ TupleT 0)

mkMockCalls :: Type -> ConLiftInfo -> Con
mkMockCalls t c =
  GadtC [callsConName c] [] (AppT t (functionCallType c))

mkMockStateCallsField :: ConLiftInfo -> (Name, Bang, Type)
mkMockStateCallsField c =
  (callsFieldName c, defaultBang, functionCallType c)

mkMockStateReturnsField :: ConLiftInfo -> (Name, Bang, Type)
mkMockStateReturnsField c =
  (returnsFieldName c, defaultBang, returnsFunctionType c)

mkInitialCalls :: ConLiftInfo -> (Name, Exp)
mkInitialCalls c =
  (callsFieldName c, ListE [])

mkInitialReturns :: ConLiftInfo -> (Name, Exp)
mkInitialReturns c =
  (returnsFieldName c, AppE (VarE 'error) (LitE (StringL "Not implemented")))

mkMockMatch :: ConLiftInfo -> Match
mkMockMatch c =
  let pat = ConP (cliConName c) (map (VarP . fst) (cliFunArgs c))
      sendFn = VarE 'send
      args = map (VarE . fst) (cliFunArgs c)
      theMock = foldl' AppE (ConE $ mockConName c) args
      body = NormalB (AppE sendFn theMock)
   in Match pat body []

mkMockToStateMatch :: ConLiftInfo -> Match
mkMockToStateMatch c =
  let pat = ConP (mockConName c) (map VarP vars)
      --
      vars = map fst (cliFunArgs c)
      arity = length $ cliFunArgs c
      newArgs = ListE [TupE (map (VarE . fst) $ cliFunArgs c)]
      oldArgs = AppE (VarE (callsFieldName c)) (VarE stateName)
      allArgs = InfixE (Just oldArgs) (VarE '(++)) (Just newArgs)
      newState = RecUpdE (VarE stateName) [(callsFieldName c, allArgs)]
      --
      applyReturnsFn = foldl' AppE (VarE (returnsFieldName c)) (VarE stateName : map VarE vars)
      embedReturnsFn = AppE (VarE 'embed) applyReturnsFn
      returnAsPureT = NoBindS $ InfixE (Just (VarE 'pureT)) (VarE '(=<<)) (Just embedReturnsFn)
      body =
        NormalB
          ( DoE
              [ getState,
                putState newState,
                returnAsPureT
              ]
          )
   in Match pat body []

mkReturnsToStateMatch :: ConLiftInfo -> Match
mkReturnsToStateMatch c =
  let f = mkName "f"
      pat = ConP (returnsConName c) [VarP f]
      newState = RecUpdE (VarE stateName) [(returnsFieldName c, VarE f)]
      returnNothing = NoBindS $ AppE (VarE 'pureT) (TupE [])
      body =
        NormalB
          ( DoE
              [ getState,
                putState newState,
                returnNothing
              ]
          )
   in Match pat body []

mkCallsToStateMatch :: ConLiftInfo -> Match
mkCallsToStateMatch c =
  let pat = ConP (callsConName c) []
      returnCalls = NoBindS $ AppE (VarE 'pureT) (AppE (VarE (callsFieldName c)) (VarE stateName))
      body =
        NormalB
          ( DoE
              [ getState,
                returnCalls
              ]
          )
   in Match pat body []

getState :: Stmt
getState = BindS (VarP stateName) (VarE 'get)

putState :: Exp -> Stmt
putState newState = NoBindS (AppE (VarE 'put) newState)

stateName :: Name
stateName = mkName "state"

callsConName :: ConLiftInfo -> Name
callsConName c = mkName ("Mock" <> nameBase (cliConName c) <> "Calls")

returnsConName :: ConLiftInfo -> Name
returnsConName c = mkName ("Mock" <> nameBase (cliConName c) <> "Returns")

mockConName :: ConLiftInfo -> Name
mockConName c = mkName ("Mock" <> nameBase (cliConName c))

callsFieldName :: ConLiftInfo -> Name
callsFieldName c = mkName (nameBase (cliFunName c) <> "Calls")

returnsFieldName :: ConLiftInfo -> Name
returnsFieldName c = mkName (nameBase (cliFunName c) <> "Returns")

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

functionCallType :: ConLiftInfo -> Type
functionCallType c =
  let arity = length $ cliFunArgs c
   in AppT ListT $ foldl' AppT (TupleT arity) (map snd $ cliFunArgs c)

returnsFunctionType :: ConLiftInfo -> Type
returnsFunctionType c =
  let argTypes = (map snd $ cliFunArgs c)
      returnType = (AppT (ConT ''IO) $ cliEffRes c)
   in foldr (AppT . AppT ArrowT) returnType argTypes

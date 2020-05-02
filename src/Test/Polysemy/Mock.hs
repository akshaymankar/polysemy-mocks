{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Polysemy.Mock
  ( Mock (..),
    runMock,
    evalMock,
    execMock,
  )
where

import Data.Kind
import Polysemy
import Polysemy.State

-- | The 'Mock' class can be instantiated for an effect 'eff' and a functor 'm'.
-- Here 'eff' represents the effect being mocked and 'm' is the side-effect the mock implementation uses to keep track of 'MockState'.
--
-- To take the classic example of Teletype, we can mock Teletype using the 'Identity' functor like this:
-- Consder a Teletype effect defined as:
--
-- @
-- data Teletype (m :: * -> *) a where
--   Read :: Teletype m String
--   Write :: String -> Teletype m ()
--
-- makeSem ''Teletype
-- @
--
-- A simple 'Mock' instance which always reads "Mock" when Read action is called and records all Write actions.
--
-- @
-- instance Mock Teletype Identity where
--   data MockImpl Teletype Identity m a where
--     MockRead :: MockImpl Teletype Identity m String
--     MockWrite :: String -> MockImpl Teletype Identity m String
--     MockWriteCalls :: MockImpl Teletype Identity m [String]
--
--   data MockState Teletype Identity = MockState {writes :: [String]}
--
--   initialMockState = MockState []
--
--   mock = interpret $ \case
--     Read -> send @(MockImpl Teletype Identity) MockRead
--     Write s -> send @(MockImpl Teletype Identity) $ MockWrite s
--
--   mockToState = reinterpretH $ \case
--     MockRead -> pureT "Mock"
--     MockWrite s -> do
--       (MockState w) <- get @(MockState Teletype Identity)
--       put $ MockState (w ++ [s])
--       pureT ()
--     MockWriteCalls -> do
--       (MockState w) <- get @(MockState Teletype Identity)
--       pureT w
-- @
--
-- If we have a program which uses the @Teletype@ effect like this:
--
-- @
-- program :: Member Teletype r => Sem r ()
-- program = do
--  name <- read
--  write $ "Hello " <> name
-- @
--
-- This program can be tested using hspec and our mock like this:
--
-- @
-- spec :: Spec
-- spec = describe "program" $ do
--   it "writes hello message" $ do
--     let MockState w =
--           runIdentity . runM . execMock $
--             mock @Teletype @Identity program
--     w `shouldBe` ["Hello Mock"]
-- @
--
-- One can write such tests without even using this class. This class and the
-- library is more useful when used with the template haskell generator for the
-- mocks. The generator will produce a different mock than written above and it
-- can be used like this:
--
-- @
-- genMock ''Teletype
--
-- mockWriteReturns :: (String -> m ()) -> Sem '[MockImpl Teletype m, Embed m] ()
-- mockWriteReturns = send . MockWriteReturns
--
-- mockReadReturns :: m String -> Sem '[MockImpl Teletype m, Embed m] ()
-- mockReadReturns = send . MockReadReturns
--
-- mockReadCalls :: forall m. Sem '[MockImpl Teletype m, Embed m] [()]
-- mockReadCalls = send @(MockImpl Teletype m) MockReadCalls
--
-- mockWriteCalls :: forall m. Sem '[MockImpl Teletype m, Embed m] [String]
-- mockWriteCalls = send @(MockImpl Teletype m) MockWriteCalls
--
-- spec :: Spec
-- spec = describe "program" $ do
--   it "writes hello message" $ runM @IO . evalMock do
--     mockReadReturns $ pure "Mock"
--     mockWriteReturns $ pure ()
--     mock @Teletype @IO program
--     w <- mockWriteCalls
--     embed $ w `shouldBe` ["Hello Mock"]
-- @
class Mock (eff :: Effect) (m :: Type -> Type) where
  -- | The effect which 'eff' should be interpreted to
  data MockImpl eff m :: Effect

  -- | The type keep information about the mock.
  -- For example, it can be used to keep record of actions called on the effect and what to return on each call
  data MockState eff m

  -- | Can be used to set default return values and initialize other attributes of the 'MockState'
  initialMockState :: MockState eff m

  -- | Swaps real effect for the mock one.
  mock :: Member (MockImpl eff m) r => Sem (eff ': r) a -> Sem r a

  -- | Update mock state for every action on the mock
  mockToState :: Member (Embed m) r => Sem (MockImpl eff m ': r) a -> Sem (State (MockState eff m) ': r) a

-- | Run a mocked effect to get 'MockState' and the effect value
runMock :: (Mock eff m, Member (Embed m) r) => Sem (MockImpl eff m ': r) a -> Sem r (MockState eff m, a)
runMock = runState initialMockState . mockToState

-- | Like 'runMock' but discards the 'MockState'
evalMock :: (Mock eff m, Member (Embed m) r) => Sem (MockImpl eff m ': r) a -> Sem r a
evalMock = evalState initialMockState . mockToState

-- | Like 'runMock' but only returns the 'MockState'
execMock :: (Mock eff m, Member (Embed m) r) => Sem (MockImpl eff m ': r) a -> Sem r (MockState eff m)
execMock = execState initialMockState . mockToState

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

import Data.Functor.Identity
import Data.Kind
import Polysemy
import Polysemy.Internal
import Polysemy.State

class Mock (x :: Effect) (m :: Type -> Type) where
  data MockImpl x m :: Effect
  data MockState x m
  initialMockState :: MockState x m
  mock :: Member (MockImpl x m) r => Sem (x ': r) a -> Sem r a
  mockToState :: Member (Embed m) r => Sem (MockImpl x m ': r) a -> Sem (State (MockState x m) ': r) a

runMock :: (Mock x m, Member (Embed m) r) => Sem (MockImpl x m ': r) a -> Sem r (MockState x m, a)
runMock = runState initialMockState . mockToState

evalMock :: (Mock x m, Member (Embed m) r) => Sem (MockImpl x m ': r) a -> Sem r a
evalMock = evalState initialMockState . mockToState

execMock :: (Mock x m, Member (Embed m) r) => Sem (MockImpl x m ': r) a -> Sem r (MockState x m)
execMock = execState initialMockState . mockToState

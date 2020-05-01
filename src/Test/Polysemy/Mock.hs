{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Polysemy.Mock where

import Polysemy
import Polysemy.State

class Mock x where
  data MockImpl x :: Effect
  data MockState x
  initialMockState :: MockState x
  mock :: Member (MockImpl x) r => Sem (x ': r) a -> Sem r a
  mockToState :: Members '[Embed IO] r => Sem (MockImpl x ': r) a -> Sem (State (MockState x) ': r) a

runMock :: (Mock x, Member (Embed IO) r) => Sem (MockImpl x ': r) a -> Sem r a
runMock = evalState initialMockState . mockToState

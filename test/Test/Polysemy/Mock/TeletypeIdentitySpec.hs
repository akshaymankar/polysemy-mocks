{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Polysemy.Mock.TeletypeIdentitySpec where

import Data.Functor.Identity
import Data.Kind
import Polysemy
import Polysemy.Internal (send)
import Polysemy.State
import Test.Hspec
import Test.Polysemy.Mock
import Prelude hiding (read)

data Teletype (m :: Type -> Type) a where
  Read :: Teletype m String
  Write :: String -> Teletype m ()

read :: Member Teletype r => Sem r String
read = send Read

write :: Member Teletype r => String -> Sem r ()
write = send . Write

instance Mock Teletype Identity where
  data MockImpl Teletype Identity m a where
    MockRead :: MockImpl Teletype Identity m String
    MockWrite :: String -> MockImpl Teletype Identity m ()
    MockWriteCalls :: MockImpl Teletype Identity m [String]

  data MockState Teletype Identity = MockState {writes :: [String]}

  initialMockState = MockState []

  mock = interpret $ \case
    Read -> send @(MockImpl Teletype Identity) MockRead
    Write s -> send @(MockImpl Teletype Identity) $ MockWrite s

  mockToState = reinterpretH $ \case
    MockRead -> pureT "Mock"
    MockWrite s -> do
      (MockState w) <- get @(MockState Teletype Identity)
      put $ MockState (w ++ [s])
      pureT ()
    MockWriteCalls -> do
      (MockState w) <- get @(MockState Teletype Identity)
      pureT w

program :: Member Teletype r => Sem r ()
program = do
  name <- read
  write $ "Hello " <> name

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec = describe "program" $ do
  it "writes hello message" $ do
    let MockState w =
          runIdentity . runM . execMock $
            mock @Teletype @Identity program
    w `shouldBe` ["Hello Mock"]

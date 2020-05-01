{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Polysemy.Mock.TeletypeSpec where

import Data.Functor.Identity
import Data.Kind
import Polysemy
import Polysemy.Internal (send)
import Polysemy.State
import Test.Hspec
import Test.Polysemy.Mock
import Prelude hiding (read)

data Teletype (m :: * -> *) a where
  Read :: Teletype m String
  Write :: String -> Teletype m ()

read :: Member Teletype r => Sem r String
read = send Read

write :: Member Teletype r => String -> Sem r ()
write = send . Write

instance forall n. Mock Teletype n where
  data MockImpl Teletype n m a where
    MockRead :: MockImpl Teletype n m String
    MockWrite :: String -> MockImpl Teletype n m ()
    MockReadReturns :: n String -> MockImpl Teletype n m ()
    MockWriteReturns :: (String -> n ()) -> MockImpl Teletype n m ()
    MockReadCalls :: MockImpl Teletype n m [()]
    MockWriteCalls :: MockImpl Teletype n m [String]

  data MockState Teletype n = MockState
    { readCalls :: [()],
      writeCalls :: [String],
      readReturns :: n String,
      writeReturns :: String -> n ()
    }

  initialMockState = MockState [] [] (error "Unimplemented") (error "Unimplemented")

  mock = interpret $ \case
    Read -> send @(MockImpl Teletype n) MockRead
    Write s -> send @(MockImpl Teletype n) $ MockWrite s
  mockToState = reinterpretH $ \case
    MockRead -> do
      state <- get @(MockState Teletype n)
      put state {readCalls = readCalls state ++ [()]}
      pureT =<< embed (readReturns state)
    MockWrite s -> do
      state <- get @(MockState Teletype n)
      put state {writeCalls = writeCalls state ++ [s]}
      pureT =<< embed (writeReturns state s)
    MockReadReturns f -> do
      state <- get @(MockState Teletype n)
      put state {readReturns = f}
      pureT ()
    MockWriteReturns f -> do
      state <- get @(MockState Teletype n)
      put state {writeReturns = f}
      pureT ()
    MockReadCalls -> do
      state <- get @(MockState Teletype n)
      pureT (readCalls state)
    MockWriteCalls -> do
      state <- get @(MockState Teletype n)
      pureT (writeCalls state)

mockWriteReturns :: (String -> m ()) -> Sem '[MockImpl Teletype m, Embed m] ()
mockWriteReturns = send . MockWriteReturns

mockReadReturns :: m String -> Sem '[MockImpl Teletype m, Embed m] ()
mockReadReturns = send . MockReadReturns

mockReadCalls :: forall m. Sem '[MockImpl Teletype m, Embed m] [()]
mockReadCalls = send @(MockImpl Teletype m) MockReadCalls

mockWriteCalls :: forall m. Sem '[MockImpl Teletype m, Embed m] [String]
mockWriteCalls = send @(MockImpl Teletype m) MockWriteCalls

program :: Member Teletype r => Sem r ()
program = do
  write "Name: "
  name <- read
  write $ "Hello " <> name

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec =
  describe "program" $ do
    it "greets" $ runM @IO . evalMock $ do
      mockWriteReturns (const $ pure ())
      mockReadReturns (pure "Akshay")
      mock @Teletype @IO program
      writeCalls <- mockWriteCalls
      embed $ writeCalls `shouldBe` ["Name: ", "Hello Akshay"]
    it "greets without IO" $ do
      let state = runIdentity . runM @Identity . execMock $ do
            mockWriteReturns (const $ pure ())
            mockReadReturns (pure "Akshay")
            mock @Teletype @Identity program
      writeCalls state `shouldBe` ["Name: ", "Hello Akshay"]

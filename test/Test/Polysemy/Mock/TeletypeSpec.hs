{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Polysemy.Mock.TeletypeSpec where

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

instance Mock Teletype where
  data MockImpl Teletype m a where
    MockRead :: MockImpl Teletype m String
    MockWrite :: String -> MockImpl Teletype m ()
    MockReadReturns :: IO String -> MockImpl Teletype m ()
    MockWriteReturns :: (String -> IO ()) -> MockImpl Teletype m ()
    MockReadCalls :: MockImpl Teletype m [()]
    MockWriteCalls :: MockImpl Teletype m [String]

  data MockState Teletype = MockState
    { readCalls :: [()],
      writeCalls :: [String],
      readReturns :: IO String,
      writeReturns :: String -> IO ()
    }

  initialMockState = MockState [] [] (error "Unimplemented") (error "Unimplemented")

  mock = interpret $ \case
    Read -> send MockRead
    Write s -> send $ MockWrite s
  mockToState = reinterpretH $ \case
    MockRead -> do
      state <- get
      put state {readCalls = readCalls state ++ [()]}
      pureT =<< embed (readReturns state)
    MockWrite s -> do
      state <- get
      put state {writeCalls = writeCalls state ++ [s]}
      pureT =<< embed (writeReturns state s)
    MockReadReturns f -> do
      state <- get
      put state {readReturns = f}
      pureT ()
    MockWriteReturns f -> do
      state <- get
      put state {writeReturns = f}
      pureT ()
    MockReadCalls -> do
      state <- get
      pureT (readCalls state)
    MockWriteCalls -> do
      state <- get
      pureT (writeCalls state)

mockWriteReturns :: (String -> IO ()) -> Sem '[MockImpl Teletype, Embed IO] ()
mockWriteReturns = send . MockWriteReturns

mockReadReturns :: IO String -> Sem '[MockImpl Teletype, Embed IO] ()
mockReadReturns = send . MockReadReturns

mockReadCalls :: Sem '[MockImpl Teletype, Embed IO] [()]
mockReadCalls = send MockReadCalls

mockWriteCalls :: Sem '[MockImpl Teletype, Embed IO] [String]
mockWriteCalls = send MockWriteCalls

program :: Member Teletype r => Sem r ()
program = do
  write "Name: "
  name <- read
  write $ "Hello " <> name

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec =
  describe "program" $ do
    it "greets" $ runM @IO . runMock $ do
      mockWriteReturns (const $ pure ())
      mockReadReturns (pure "Akshay")
      mock @Teletype program
      writeCalls <- mockWriteCalls
      embed $ writeCalls `shouldBe` ["Name: ", "Hello Akshay"]

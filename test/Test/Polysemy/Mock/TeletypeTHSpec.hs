{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Polysemy.Mock.TeletypeTHSpec where

import Data.Kind
import Polysemy
import Test.Hspec
import Test.Polysemy.Mock
import Test.Polysemy.Mock.TH (genMock)
import Prelude hiding (read)

data Teletype (m :: * -> *) a where
  Read :: Teletype m String
  Write :: String -> Teletype m ()

makeSem ''Teletype

genMock ''Teletype

makeSem 'MockRead

program :: Member Teletype r => Sem r ()
program = do
  write "Name: "
  name <- read
  write $ "Hello " <> name

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec =
  describe "program" $ do
    it "greets" $ runM @IO . runMock @Teletype $ do
      mockWriteReturns (const $ pure ())
      mockReadReturns (pure "Akshay")
      mock @Teletype program
      writeCalls <- mockWriteCalls
      embed $ writeCalls `shouldBe` ["Name: ", "Hello Akshay"]

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

data Teletype (m :: Type -> Type) a where
  Read :: Teletype m String
  Write :: String -> Teletype m ()

makeSem ''Teletype

genMock ''Teletype

program :: Member Teletype r => Sem r ()
program = do
  write "Name: "
  name <- read
  write $ "Hello " <> name

{-# ANN spec ("HLint: ignore Redundant do" :: String) #-}
spec :: Spec
spec =
  describe "program" $ do
    it "greets" $
      runM @IO . evalMock @Teletype @IO $ do
        mockReadReturns @IO (pure "Akshay")
        mock @Teletype @IO program
        writes <- mockWriteCalls @IO
        embed $ writes `shouldBe` ["Name: ", "Hello Akshay"]

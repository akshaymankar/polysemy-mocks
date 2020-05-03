# Polysemy Mocks

## Overview

`polysemy-mocks` aims to provide a structure to write all mocks for polysemy and to generate those.

## Example

```haskell
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

module TeletypeSpec where

import Data.Kind
import Polysemy
import Polysemy.Internal (send)
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

spec :: Spec
spec =
  describe "program" $ do
    it "greets" $ runM @IO . evalMock $ do
      mockReadReturns (pure "Akshay")
      mock @Teletype @IO program
      writes <- mockWriteCalls
      embed $ writes `shouldBe` ["Name: ", "Hello Akshay"]
```

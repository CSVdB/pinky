{-# LANGUAGE TypeApplications #-}

module Test.Neural.ParamOpt.InstanceSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import Neural

import Test.Neural.ParamOpt.Gen ()

spec :: Spec
spec = do
    eqSpec @ClassificationAccuracy
    ordSpec @ClassificationAccuracy
    genValidSpec @ClassificationAccuracy

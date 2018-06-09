{-# LANGUAGE TypeApplications #-}

module Test.Pinky.ParamOpt.InstanceSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import Pinky

import Test.Pinky.ParamOpt.Gen ()

spec :: Spec
spec = do
    eqSpec @ClassificationAccuracy
    ordSpec @ClassificationAccuracy
    genValidSpec @ClassificationAccuracy

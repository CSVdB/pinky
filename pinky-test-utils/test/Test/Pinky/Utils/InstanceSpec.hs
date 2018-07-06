{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Pinky.Utils.InstanceSpec where

import Import

import Pinky

import Test.Hspec
import Test.Pinky.Utils.Gen ()
import Test.Validity

spec :: Spec
spec = do
    eqSpec @PositiveDouble
    ordSpec @PositiveDouble
    genValidSpec @PositiveDouble
    eqSpec @ProperFraction
    ordSpec @ProperFraction
    genValidSpec @ProperFraction
    genValidSpec @(MyVec 5 Double)
    functorSpec @(MyVec 5)
    applicativeSpec @(MyVec 5)
    monadSpec @(MyVec 5)

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Pinky.Utils.InstanceSpec where

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

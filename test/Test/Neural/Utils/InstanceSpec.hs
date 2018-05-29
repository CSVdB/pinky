{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Neural.Utils.InstanceSpec where

import Neural

import Test.Hspec
import Test.Neural.Utils.Gen ()
import Test.Validity

spec :: Spec
spec = do
    eqSpec @PositiveDouble
    ordSpec @PositiveDouble
    genValidSpec @PositiveDouble
    eqSpec @ProperFraction
    ordSpec @ProperFraction
    genValidSpec @ProperFraction

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Pinky.Core.InstanceSpec where

import Pinky

import Test.Pinky.Core.Gen ()

import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
    eqSpec @HyperParams
    genValidSpec @HyperParams
    genValidSpec @(S ('D1 5))
    genValidSpec @(S ('D2 5 7))
    genValidSpec @(S ('D3 5 7 6))

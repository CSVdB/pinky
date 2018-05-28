{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Neural.Core.InstanceSpec where

import Neural

import Neural.Core.Gen ()

import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
    eqSpec @HyperParams
    genValidSpec @HyperParams
    genValidSpec @(S ('D1 5))
    genValidSpec @(S ('D2 5 7))
    genValidSpec @(S ('D3 5 7 6))

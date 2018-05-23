{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Neural.Shape.InstanceSpec where

import Neural.Shape
import Neural.Shape.Gen ()

import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
    genValidSpec @(S ('D1 5))
    genValidSpec @(S ('D2 5 7))
    genValidSpec @(S ('D3 5 7 6))

{-# LANGUAGE TypeApplications #-}

module Test.Neural.Layers.LayerSpec where

import Test.Hspec
import Test.Neural.Layers.Gen ()
import Test.Neural.Spec.Layer
import TestImport

import Neural

spec :: Spec
spec = do
    layerSpec @FCL @IShape @OShape
    layerSpec @Reshape @ImageShape @IShape

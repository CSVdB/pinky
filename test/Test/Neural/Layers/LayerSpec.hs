{-# LANGUAGE TypeApplications #-}

module Test.Neural.Layers.LayerSpec where

import Test.Hspec
import Test.Neural.Layers.Gen ()
import Test.Neural.Spec.Layer
import TestImport

spec :: Spec
spec = layerSpec @FCL @IShape @OShape

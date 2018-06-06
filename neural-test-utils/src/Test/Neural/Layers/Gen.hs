{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.Gen
    (
    ) where

import Test.Neural.Core.Gen ()
import Test.Neural.Layers.FullyConnected ()
import Test.Neural.Layers.Reshape ()
import Test.Neural.Layers.Resize ()
import Test.Neural.Layers.Sigmoid ()

import Import

import Neural

instance GenUnchecked x => GenUnchecked (Gradient x)

instance GenValid x => GenValid (Gradient x)

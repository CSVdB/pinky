{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.Gen
    (
    ) where

import Test.Pinky.Core.Gen ()
import Test.Pinky.Layers.Elu ()
import Test.Pinky.Layers.FullyConnected ()
import Test.Pinky.Layers.Relu ()
import Test.Pinky.Layers.Reshape ()
import Test.Pinky.Layers.Resize ()
import Test.Pinky.Layers.Sigmoid ()
import Test.Pinky.Layers.Softmax ()

import Import

import Pinky

instance GenUnchecked x => GenUnchecked (Gradient x)

instance GenValid x => GenValid (Gradient x)

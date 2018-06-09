{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.FullyConnected where

import Import
import Test.Pinky.Core.LinearAlgebraGen ()

import Pinky

instance (KnownNat i, KnownNat o) => GenUnchecked (FullyConnected i o)

instance (KnownNat i, KnownNat o) => GenValid (FullyConnected i o)

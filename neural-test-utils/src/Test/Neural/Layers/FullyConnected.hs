{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.FullyConnected where

import Import
import Test.Neural.Core.LinearAlgebraGen ()

import Neural

instance (KnownNat i, KnownNat o) => GenUnchecked (FullyConnected i o)

instance (KnownNat i, KnownNat o) => GenValid (FullyConnected i o)

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.FullyConnected where

import Test.Neural.Core.LinearAlgebraGen ()
import TestImport

import Neural

instance (KnownNat i, KnownNat o) => GenUnchecked (FullyConnected i o)

instance (KnownNat i, KnownNat o) => GenValid (FullyConnected i o)

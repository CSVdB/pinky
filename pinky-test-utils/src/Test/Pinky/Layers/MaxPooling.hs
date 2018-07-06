{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.MaxPooling where

import Import

import Pinky

instance (KnownNat a, KnownNat b) => GenUnchecked (MaxPooling a b) where
    genUnchecked = pure MaxPooling
    shrinkUnchecked = const []

instance (KnownNat a, KnownNat b) => GenValid (MaxPooling a b) where
    genValid = pure MaxPooling

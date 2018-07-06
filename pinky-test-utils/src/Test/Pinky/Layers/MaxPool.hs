{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pinky.Layers.MaxPool where

import Import

import Test.Massiv ()
import Test.QuickCheck

import Pinky
import Pinky.Layers.MaxPool.Internal

import Data.Massiv.Array

instance (KnownNat a, KnownNat b) => GenUnchecked (MaxPool a b) where
    genUnchecked = pure MaxPool
    shrinkUnchecked = const []

instance (KnownNat a, KnownNat b) => GenValid (MaxPool a b) where
    genValid = pure MaxPool

instance GenUnchecked (MaxPoolTape x y c) where
    genUnchecked = do
        xDim <- (+ 1) . abs <$> genUnchecked
        yDim <- (+ 1) . abs <$> genUnchecked
        zDim <- (+ 1) . abs <$> genUnchecked
        comp <- genUnchecked
        MaxPoolTape . fromLists' comp <$>
            replicateM xDim (replicateM yDim $ replicateM zDim genUnchecked)
    shrinkUnchecked = const []

genUnderIx3 :: Ix3 -> Gen Ix3
genUnderIx3 (a :> b :. c) = do
    x <- choose (0, a - 1)
    y <- choose (0, b - 1)
    z <- choose (0, c - 1)
    pure $ x :> y :. z

instance (KnownNat x, KnownNat y, KnownNat z) =>
         GenValid (MaxPoolTape x y z) where
    genValid = do
        let xDim = natToInt @x
        let yDim = natToInt @y
        let zDim = natToInt @z
        let ix = xDim :> yDim :. zDim
        comp <- genUnchecked
        MaxPoolTape . fromLists' comp <$>
            replicateM
                (natToInt @x)
                (replicateM (natToInt @y) $
                 replicateM (natToInt @z) $ genUnderIx3 ix)

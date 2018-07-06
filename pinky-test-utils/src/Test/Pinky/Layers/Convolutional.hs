{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pinky.Layers.Convolutional where

import Import

import Test.Pinky.Core.LinearAlgebraGen ()
import Test.QuickCheck

import Pinky

import qualified Data.Vector.Storable as SV

import Data.Massiv.Array
import Data.Massiv.Array.Manifest.Vector

genPosInt :: Gen Int
genPosInt = abs <$> genValid `suchThat` (> 0)

instance ( KnownNat c
         , KnownNat c'
         , KnownNat s
         , KnownNat s'
         , KnownNat k
         , KnownNat k'
         ) =>
         GenUnchecked (Convolutional c c' s s' k k') where
    genUnchecked = do
        kN <- oneof [pure $ natToInt @k, genPosInt]
        kN' <- oneof [pure $ natToInt @k', genPosInt]
        genConv kN kN' genUnchecked
    shrinkUnchecked = const []

instance ( KnownNat c
         , KnownNat c'
         , KnownNat s
         , KnownNat s'
         , KnownNat k
         , KnownNat k'
         ) =>
         GenValid (Convolutional c c' s s' k k') where
    genValid = genConv (natToInt @k) (natToInt @k') genValid

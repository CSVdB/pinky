{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.Pinky.Layers.ConvAlt where

import Import

import Test.Pinky.Core.LinearAlgebraGen ()
import Test.QuickCheck

import Pinky

import Data.Massiv.Core.Index (Ix2(..), IxN(..))

genPosInt :: Gen Int
genPosInt = (+ 1) . abs <$> genValid

type ConvConstraints chanIn chanOut strideX strideY kernelX kernelY
     = ( KnownNat chanIn
       , KnownNat chanOut
       , KnownNat strideX
       , KnownNat strideY
       , KnownNat kernelX
       , KnownNat kernelY)

instance ConvConstraints c c' s s' k k' =>
         GenUnchecked (ConvAlt c c' s s' k k') where
    genUnchecked = do
        kN <- oneof [pure $ natToInt @k, genPosInt]
        kN' <- oneof [pure $ natToInt @k', genPosInt]
        cN <- oneof [pure $ natToInt @c, genPosInt]
        genConvAlt (kN :> kN' :. cN) genUnchecked
    shrinkUnchecked = const []

instance ConvConstraints c c' s s' k k' =>
         GenValid (ConvAlt c c' s s' k k') where
    genValid =
        genConvAlt (natToInt @k :> natToInt @k' :. natToInt @c) $ choose (-1, 1)

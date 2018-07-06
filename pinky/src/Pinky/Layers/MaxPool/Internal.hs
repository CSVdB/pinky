{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pinky.Layers.MaxPool.Internal where

import Import

import Data.Massiv.Array
import Data.Massiv.Core
import Pinky.Utils

newtype MaxPoolTape (x :: Nat) (y :: Nat) (c :: Nat) =
    MaxPoolTape (Array U Ix3 Ix3)
    deriving (Show, Eq)

instance (KnownNat x, KnownNat y, KnownNat z) =>
         Validity (MaxPoolTape x y z) where
    validate (MaxPoolTape arr) =
        mconcat
            [ declare "MaxPoolTape must have the right size" $
              size arr == natToInt @x :> natToInt @y :. natToInt @z
            , delve "Internal array" arr
            ]

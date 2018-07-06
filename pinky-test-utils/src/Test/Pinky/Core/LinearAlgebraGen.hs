{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pinky.Core.LinearAlgebraGen where

import Import

import Pinky.Core.LinearAlgebra.Internal
import Pinky.Utils

import qualified Data.Vector.Storable as SV
import Numeric.LinearAlgebra.Data (reshape)
import qualified Numeric.LinearAlgebra.Static as Hmatrix

import Test.QuickCheck

doubleGenToRGen ::
       forall n. KnownNat n
    => Gen Double
    -> Gen (Hmatrix.R n)
doubleGenToRGen doubleGen =
    let i = natToInt @n
     in (SV.fromList <$> replicateM i doubleGen) `suchThatMap` Hmatrix.create

instance KnownNat n => GenUnchecked (Hmatrix.R n) where
    genUnchecked = doubleGenToRGen genUnchecked
    shrinkUnchecked = const []

instance KnownNat n => GenValid (Hmatrix.R n) where
    genValid = doubleGenToRGen genValid

instance KnownNat n => GenUnchecked (V n) where
    genUnchecked = V <$> genUnchecked

instance KnownNat n => GenValid (V n) where
    genValid = V <$> genValid

doubleGenToLGen ::
       forall i j. (KnownNat i, KnownNat j)
    => Gen Double
    -> Gen (Hmatrix.L i j)
doubleGenToLGen doubleGen =
    let i' = natToInt @i
        j' = natToInt @j
     in (reshape j' . SV.fromList <$> replicateM (i' * j') doubleGen) `suchThatMap`
        Hmatrix.create

instance (KnownNat i, KnownNat j) => GenUnchecked (Hmatrix.L i j) where
    genUnchecked = doubleGenToLGen genUnchecked
    shrinkUnchecked = const []

instance (KnownNat i, KnownNat j) => GenValid (Hmatrix.L i j) where
    genValid = doubleGenToLGen genValid

instance (KnownNat i, KnownNat j) => GenUnchecked (M i j) where
    genUnchecked = M <$> genUnchecked

instance (KnownNat i, KnownNat j) => GenValid (M i j) where
    genValid = M <$> genValid

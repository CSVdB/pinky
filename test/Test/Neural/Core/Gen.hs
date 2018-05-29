{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Neural.Core.Gen where

import Test.QuickCheck
import TestImport

import Neural
import Neural.HyperParams.Internal
import Neural.PositiveDouble.Internal
import Test.Neural.LinearAlgebra.Gen ()
import Test.Neural.Utils.Gen ()

instance GenUnchecked Natural where
    genUnchecked = naturalFromInteger . abs <$> genUnchecked
    shrinkUnchecked n = [1 .. n]

instance GenValid Natural where
    genValid = naturalFromInteger . abs <$> genValid

instance GenUnchecked HyperParams

instance GenValid HyperParams where
    genValid = gen `suchThat` isValid
      where
        gen = do
            rate <- genValid
            dr <- genValid
            mom <- genValid
            reg <- PositiveDouble <$> choose (0, posToDouble rate)
            HyperParams rate dr mom reg <$> genValid

instance SingI x => GenUnchecked (S x) where
    genUnchecked =
        case sing :: Sing x of
            D1Sing SNat -> S1D <$> genUnchecked
            D2Sing SNat SNat -> S2D <$> genUnchecked
            D3Sing SNat SNat SNat -> S3D <$> genUnchecked
    shrinkUnchecked = const []

instance SingI x => GenValid (S x) where
    genValid =
        case sing :: Sing x of
            D1Sing SNat -> S1D <$> genValid
            D2Sing SNat SNat -> S2D <$> genValid
            D3Sing SNat SNat SNat -> S3D <$> genValid

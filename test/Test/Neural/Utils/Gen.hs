{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Utils.Gen where

import TestImport

import Neural
import Neural.Utils.PositiveDouble.Internal
import Neural.Utils.ProperFraction.Internal

import Test.QuickCheck

instance GenUnchecked PositiveDouble where
    genUnchecked = PositiveDouble <$> genUnchecked

instance GenValid PositiveDouble where
    genValid = PositiveDouble . abs <$> genValid

instance GenUnchecked ProperFraction where
    genUnchecked = ProperFraction <$> genUnchecked

instance GenValid ProperFraction where
    genValid = ProperFraction <$> choose (0, 1)

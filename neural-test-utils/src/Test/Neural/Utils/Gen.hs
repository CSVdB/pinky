{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Utils.Gen where

import Import
import Test.QuickCheck

import Neural
import Neural.Utils.PositiveDouble.Internal
import Neural.Utils.PositiveInt.Internal
import Neural.Utils.ProperFraction.Internal

instance GenUnchecked PositiveDouble where
    genUnchecked = PositiveDouble <$> genUnchecked

instance GenValid PositiveDouble where
    genValid = PositiveDouble . abs <$> genValid

instance GenUnchecked Natural where
    genUnchecked = naturalFromInteger . abs <$> genUnchecked
    shrinkUnchecked n = [1 .. n]

instance GenValid Natural where
    genValid = naturalFromInteger . abs <$> genValid

instance GenUnchecked PositiveInt where
    genUnchecked = PositiveInt <$> genUnchecked

instance GenValid PositiveInt where
    genValid = PositiveInt . abs <$> genValid `suchThat` isValid

instance GenUnchecked ProperFraction where
    genUnchecked = ProperFraction <$> genUnchecked

instance GenValid ProperFraction where
    genValid = ProperFraction <$> choose (0, 1)

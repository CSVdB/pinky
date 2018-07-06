{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pinky.Utils.Gen where

import Import
import Test.QuickCheck

import Pinky
import Pinky.Utils.MyVec.Internal
import Pinky.Utils.PositiveDouble.Internal
import Pinky.Utils.PositiveInt.Internal
import Pinky.Utils.ProperFraction.Internal

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

instance (KnownNat n, GenUnchecked a) => GenUnchecked (MyVec n a) where
    genUnchecked = do
        n' <- oneof [pure $ natToInt @n, (+) 1 . abs <$> genValid]
        MyVec <$> replicateM n' genUnchecked

instance (KnownNat n, GenValid a) => GenValid (MyVec n a) where
    genValid = MyVec <$> replicateM (natToInt @n) genUnchecked

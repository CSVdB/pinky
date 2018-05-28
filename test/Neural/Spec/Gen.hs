{-# OPTIONS_GHC -Wno-orphans #-}

module Neural.Spec.Gen where

import TestImport

import System.Random

instance Validity StdGen where
    validate = trivialValidation

instance GenUnchecked StdGen where
    genUnchecked = mkStdGen <$> genUnchecked
    shrinkUnchecked = const []

instance GenValid StdGen where
    genValid = mkStdGen <$> genValid

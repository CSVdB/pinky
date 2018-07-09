{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.HyperOpt.Gen where

import Import

import Pinky
import Test.Pinky.ParamOpt.Gen ()

import Test.QuickCheck

instance GenUnchecked UpdateHyperParams

instance GenValid UpdateHyperParams

instance (SingI i, SingI o) => GenUnchecked (SearchInfo i o)

genSmallNatural :: Int -> Gen Natural
genSmallNatural n = fromIntegral <$> choose (1, n)

instance (SingI i, SingI o) => GenValid (SearchInfo i o) where
    genValid =
        SearchInfo <$> genValid <*> genSmallNatural 10 <*> genValid <*> genValid

instance (SingI i, SingI o) => GenUnchecked (LocalSearchArgs i o)

instance (SingI i, SingI o) => GenValid (LocalSearchArgs i o) where
    genValid =
        LocalSearchArgs <$> genValid <*> genValid <*> genValid <*> genValid

instance (SingI i, SingI o) => GenUnchecked (RandomSearchArgs i o)

instance (SingI i, SingI o) => GenValid (RandomSearchArgs i o) where
    genValid = RandomSearchArgs <$> genValid <*> genValid

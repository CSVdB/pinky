{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.Elu where

import Import

import Pinky

instance GenUnchecked Elu

instance GenValid Elu where
    genValid = Elu . abs <$> genValid

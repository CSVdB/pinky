{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.Sigmoid where

import Import

import Pinky

instance GenUnchecked Sigmoid

instance GenValid Sigmoid

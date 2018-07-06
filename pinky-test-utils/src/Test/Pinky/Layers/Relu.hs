{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.Relu where

import Import

import Pinky

instance GenUnchecked Relu

instance GenValid Relu

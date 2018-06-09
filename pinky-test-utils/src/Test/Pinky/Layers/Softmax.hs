{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.Softmax where

import Import

import Pinky

instance GenUnchecked Softmax

instance GenValid Softmax

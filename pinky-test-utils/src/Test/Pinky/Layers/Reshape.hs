{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.Reshape where

import Import

import Pinky

instance GenUnchecked Reshape

instance GenValid Reshape

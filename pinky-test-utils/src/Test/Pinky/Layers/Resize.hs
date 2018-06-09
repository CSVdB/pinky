{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Pinky.Layers.Resize where

import Import

import Pinky

instance GenUnchecked Resize

instance GenValid Resize

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.Sigmoid where

import Import

import Neural

instance GenUnchecked Sigmoid where
    genUnchecked = pure Sigmoid
    shrinkUnchecked = const []

instance GenValid Sigmoid where
    genValid = pure Sigmoid

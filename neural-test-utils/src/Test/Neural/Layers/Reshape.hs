{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.Reshape where

import Import

import Neural

instance GenUnchecked Reshape where
    genUnchecked = pure Reshape
    shrinkUnchecked = const []

instance GenValid Reshape where
    genValid = pure Reshape

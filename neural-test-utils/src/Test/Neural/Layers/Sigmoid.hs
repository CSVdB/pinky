{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.Sigmoid where

import Import

import Neural

instance GenUnchecked Sigmoid

instance GenValid Sigmoid

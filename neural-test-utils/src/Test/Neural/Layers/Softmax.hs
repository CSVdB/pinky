{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.Softmax where

import Import

import Neural

instance GenUnchecked Softmax

instance GenValid Softmax

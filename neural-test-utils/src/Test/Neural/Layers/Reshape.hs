{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.Reshape where

import Import

import Neural

instance GenUnchecked Reshape

instance GenValid Reshape

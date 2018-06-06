{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.Layers.Resize where

import Import

import Neural

instance GenUnchecked Resize

instance GenValid Resize

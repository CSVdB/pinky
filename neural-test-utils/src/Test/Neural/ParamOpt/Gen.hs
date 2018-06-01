{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.ParamOpt.Gen where

import Import
import Test.Neural.Utils.Gen ()

import Neural

instance GenUnchecked ClassificationAccuracy

instance GenValid ClassificationAccuracy

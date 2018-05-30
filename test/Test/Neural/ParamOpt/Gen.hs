{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Neural.ParamOpt.Gen where

import Test.Neural.Utils.Gen ()
import TestImport

import Neural

instance GenUnchecked ClassificationAccuracy

instance GenValid ClassificationAccuracy

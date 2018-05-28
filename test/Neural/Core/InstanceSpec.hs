{-# LANGUAGE TypeApplications #-}

module Neural.Core.InstanceSpec where

import TestImport

import Neural

import Neural.Core.Gen ()

import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
    eqSpec @HyperParams
    genValidSpec @HyperParams

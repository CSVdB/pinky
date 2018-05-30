{-# LANGUAGE TypeApplications #-}

module Test.Neural.ParamOpt.Spec
    ( spec
    ) where

import Test.Hspec
import Test.Validity
import TestImport

import Neural

import Test.Neural.Layers.Gen ()

spec :: Spec
spec = do
    describe
        "applyGradientToNetwork :: NNet -> Grad (NNet) -> HyperParams -> NNet" $
        it "produces valids on valids" $
        forAllValid @NNet $ \net ->
            forAllValid $ \grad ->
                forAllValid $ \hp ->
                    shouldBeValid $ applyGradientToNetwork net grad hp
    describe "getGradientOfNetwork :: NNet -> S i -> S o -> Gradient (NNet)" $
        it "produces valids on valids" $
        forAllValid @NNet $ \net ->
            forAllValid $ \inpt ->
                forAllValid $ \label ->
                    shouldBeValid $ getGradientOfNetwork net inpt label
    describe "runIteration :: NNet -> DataSet -> HyperParams -> NNet" $
        it "produces valids on valids" $
        forAllValid @NNet $ \net ->
            forAllValid $ \dataset ->
                forAllValid $ \hp -> shouldBeValid $ runIteration net dataset hp

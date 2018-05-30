{-# LANGUAGE TypeApplications #-}

module Test.Neural.ParamOpt.ParamOptSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity
import TestImport

import Neural

import Test.Neural.Layers.Gen ()

import Control.Monad.State.Lazy

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
    describe "runIteration :: NNet -> DataSet -> State HyperParams NNet" $
        it "produces valids on valids" $
        forAllValid @NNet $ \net ->
            forAllValid $ \dataset ->
                forAllValid $ \hp ->
                    shouldBeValid $ flip evalState hp $ runIteration net dataset
    describe
        "trainNetwork :: NNet -> DataSet -> Natural -> State HyperParams NNet" $
        it "produces valids on valids" $
        forAllValid @NNet $ \net ->
            forAllValid $ \dataset ->
                forAllValid $ \epochs ->
                    forAllValid $ \hp ->
                        shouldBeValid $
                        flip evalState hp $ trainNetwork net dataset epochs
    describe "accuracy :: NNet -> DataSet -> ClassificationAccuracy" $
        it "produces valids on valids" $
        forAllValid @NNet $ \net ->
            forAllValid $ \dataset -> shouldBeValid $ accuracy net dataset

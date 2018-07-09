{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Pinky.HyperOpt.TrainSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck (forAllShrink)
import Test.Validity
import TestImport

import Pinky
import Test.Pinky.Gen

import Control.Monad.Trans.Random
import System.Random

type Input = 'D2 5 5

type Output = 'D1 3

type Net
     = Network '[ Reshape, FullyConnected 25 7, Sigmoid, FullyConnected 7 3, Softmax] '[ 'D2 5 5, 'D1 25, 'D1 7, 'D1 7, 'D1 3, 'D1 3]

spec :: Spec
spec = do
    describe "runHyperParams" $
        it "doesn't crash" $
        forAllValid $ \hp ->
            forAllValid @(SearchInfo Input Output) $ \si ->
                forAllValid @StdGen $ \seed ->
                    shouldBeValid $
                    flip evalRand seed $ runHyperParams @Net hp si
    describe "Local search" $
        it "doesn't crash" $
        forAllValid $ \hp ->
            forAllValid $ \lsa ->
                forAllShrink (genSmallNatural 10) shrinkValid $ \maxIter ->
                    forAllValid @StdGen $ \seed ->
                        let (acc, seed') =
                                flip runRand seed $
                                runHyperParams @Net hp $ lsInfo lsa
                         in shouldBeValid $
                            flip evalRand seed' $
                            localSearchHyperOpt @Net (hp, acc) maxIter lsa
    describe "random search" $
        it "doesn't crash" $
        forAllValid $ \hp ->
            forAllValid $ \rsa ->
                forAllShrink (genSmallNatural 10) shrinkValid $ \maxIter ->
                    forAllValid @StdGen $ \seed ->
                        shouldBeValid $
                        flip evalRand seed $
                        randomSearchHyperOpt @Net hp maxIter rsa

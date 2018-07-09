{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec where

import Test.Hspec

import TestImport

import Pinky

import NN.Network

import MNIST.Load

import Control.Monad.Random.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader

nOfTrain, nOfVal, nOfTest :: Int
nOfTrain = 3500

nOfVal = 500

nOfTest = 500

epochs :: Natural
epochs = 2

unsafeConstructAcc :: Double -> ClassificationAccuracy
unsafeConstructAcc x =
    case constructAccuracy x of
        Left errMess -> error errMess
        Right y -> y

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "MNIST" $ do
        describe "Parameter training" $ do
            it "unit test network training with sumSquareError" $
                nnMnistTest 0.81 SumSquareError
            it "unit test network training with crossEntropyError" $
                nnMnistTest 0.79 CrossEntropyError
            -- Put the following tests back once you implemented automated
            -- hyperparameter optimisation.
            --it "unit test network training with exponentialError" $
            --    nnMnistTest 0.70 $ exponentialError 1
        describe "HyperParameter training" $ do
            it "random search algorithm" $
                randomSearchHyperOptTest 0.78 SumSquareError
            it "local search algorithm" $
                localSearchHyperOptTest 0.75 SumSquareError

nnMnistTest :: Double -> ErrorFunc ('D1 10) -> Expectation
nnMnistTest minAccD errFunc = do
    let stdGen = mkStdGen 4
    let momNet = evalRand (createRandomM @(Rand StdGen) @(Momentum NN)) stdGen
    (!trainSet, _, testSet) <- load nOfTrain nOfVal nOfTest
    let !(Momentum trainedNet _) =
            flip runReader errFunc $
            evalStateT (trainNetwork momNet trainSet epochs) params
    let testAcc = accuracy trainedNet testSet
    let minAcc = unsafeConstructAcc minAccD
    print testAcc
    testAcc `shouldSatisfy` (> minAcc)

iter :: Natural
iter = 10

paramExpVal, paramExpValD :: Double
paramExpVal = 7

paramExpValD = 0.9

decayRateExpVal, decayRateExpValD :: Double
decayRateExpVal = 2

decayRateExpValD = 0.9

batchSizeFactorVal, batchSizeFactorValD :: Natural
batchSizeFactorVal = 5

batchSizeFactorValD = 2

randomSearchHyperOptTest :: Double -> ErrorFunc ('D1 10) -> Expectation
randomSearchHyperOptTest minAccD errFunc = do
    let minAcc = unsafeConstructAcc minAccD
    (!trainSet, _, testSet) <- load nOfTrain nOfVal nOfTest
    let si = SearchInfo errFunc epochs trainSet testSet
    let uhp = UpdateHyperParams paramExpVal decayRateExpVal batchSizeFactorVal
    let rsa = RandomSearchArgs si uhp
    (bestHp, bestAcc) <- randomSearchHyperOpt @NN initParams iter rsa
    print bestHp
    print bestAcc
    bestAcc `shouldSatisfy` (> minAcc)

localSearchHyperOptTest :: Double -> ErrorFunc ('D1 10) -> Expectation
localSearchHyperOptTest minAccD errFunc = do
    let minAcc = unsafeConstructAcc minAccD
    (!trainSet, _, testSet) <- load nOfTrain nOfVal nOfTest
    let si = SearchInfo errFunc epochs trainSet testSet
    let uhp = UpdateHyperParams paramExpVal decayRateExpVal batchSizeFactorVal
    let uhpDecay =
            UpdateHyperParams paramExpValD decayRateExpValD batchSizeFactorValD
    let lsa = LocalSearchArgs si minAcc uhp uhpDecay
    initAcc <- runHyperParams @NN initParams si
    (bestHp, bestAcc) <- localSearchHyperOpt @NN (initParams, initAcc) iter lsa
    print bestHp
    print bestAcc
    bestAcc `shouldSatisfy` (> minAcc)

initParams :: HyperParams
initParams = unsafeConstructHP 1 0.9 1e-8 1e-8 10

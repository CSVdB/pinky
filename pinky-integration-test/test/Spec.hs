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
        it "unit test network training with sumSquareError" $
            nnMnistTest 0.81 sumSquareError
        it "unit test network training with crossEntropyError" $
            nnMnistTest 0.79 crossEntropyError
        -- Put the following tests back once you implemented automated
        -- hyperparameter optimisation.
        --it "unit test network training with exponentialError" $
        --    nnMnistTest 0.70 $ exponentialError 1

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

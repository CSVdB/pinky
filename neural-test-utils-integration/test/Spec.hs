{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec where

import Test.Hspec
import Test.Neural.Spec.Gen
import Test.Validity
import TestImport

import Neural

import NN.Network

import MNIST.Load

import Control.Monad.Random.Lazy
import Control.Monad.State.Lazy

import System.Random

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
    describe "MNIST" $
    it "unit test network training" $ do
        stdGen <- getStdGen
        let net = evalRand (createRandomM @(Rand StdGen) @NN) stdGen
        (!trainSet, valSet, testSet) <- load nOfTrain nOfVal nOfTest
        let !trainedNet = evalState (trainNetwork net trainSet epochs) params
        let testAcc = accuracy trainedNet testSet
        let minAcc = unsafeConstructAcc 0.79
        testAcc `shouldSatisfy` (> minAcc)

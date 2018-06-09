{-# LANGUAGE TypeApplications #-}
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
        let stdGen = mkStdGen 4
        let momNet =
                evalRand (createRandomM @(Rand StdGen) @(Momentum NN)) stdGen
        (!trainSet, _, testSet) <- load nOfTrain nOfVal nOfTest
        let !(Momentum trainedNet _) =
                evalState (trainNetwork momNet trainSet epochs) params
        let testAcc = accuracy trainedNet testSet
        let minAcc = unsafeConstructAcc 0.81
        testAcc `shouldSatisfy` (> minAcc)

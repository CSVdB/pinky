{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec where

import Test.Hspec

import TestImport

import Pinky

import qualified CNN.Network as CNN
import qualified NN.Network as NN

import MNIST.Load

import Control.Monad.Random.Lazy
import Control.Monad.State.Lazy

nOfTrain, nOfVal, nOfTest :: Int
nOfTrain = 3500

nOfTrain' = 350

nOfVal = 500

nOfVal' = 50

nOfTest = 500

nOfTest' = 50

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
        it "unit test NN training" $ do
            let stdGen = mkStdGen 4
            let momNet =
                    evalRand
                        (createRandomM @(Rand StdGen) @(Momentum NN.NN))
                        stdGen
            (!trainSet, _, testSet) <- load nOfTrain nOfVal nOfTest
            let !(Momentum trainedNet _) =
                    evalState (trainNetwork momNet trainSet epochs) NN.params
            let testAcc = accuracy trainedNet testSet
            let minAcc = unsafeConstructAcc 0.81
            testAcc `shouldSatisfy` (> minAcc)
        it "unit test CNN training" $ do
            let stdGen = mkStdGen 4
            let momNet =
                    evalRand
                        (createRandomM @(Rand StdGen) @(Momentum CNN.CNN))
                        stdGen
            (!trainSet, _, testSet) <- load nOfTrain' nOfVal' nOfTest'
            let !(Momentum trainedNet _) =
                    evalState (trainNetwork momNet trainSet epochs) CNN.params
            let testAcc = accuracy trainedNet testSet
            let minAcc = unsafeConstructAcc 0.81
            testAcc `shouldSatisfy` (> minAcc)

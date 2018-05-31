{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Import

import Neural

import NN.Network

import MNIST.Load

import Control.Monad.State.Lazy

nOfTrain, nOfVal, nOfTest :: Int
nOfTrain = 1000

nOfVal = 1000

nOfTest = 1000

epochs :: Natural
epochs = 10

main :: IO ()
main = do
    net <- createRandomM @IO @NN
    (!trainSet, valSet, testSet) <- load nOfTrain nOfVal nOfTest
    putStrLn "Trainset loaded"
    let !trainedNet = evalState (trainNetwork net trainSet epochs) params
    printAccs trainedNet trainSet valSet testSet
  where
    printAccs network trainSet' valSet' testSet' =
        mapM_ putStrLn $
        uncurry (showAccuracyFromNetwork network) <$>
        [("train", trainSet'), ("validation", valSet'), ("test", testSet')]

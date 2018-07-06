{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main
    ( main
    ) where

import Criterion.Main

import NN.Network
import Pinky

import Data.GenValidity

import Test.Pinky.Layers.Gen ()
import Test.QuickCheck (generate)

import GHC.Natural
import GHC.TypeLits

import MNIST.Load

import Control.Monad.State.Lazy

import GHC.IO.Encoding

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain
        [ bench "Generate input" $ eval genInputShape
        , bench "Generate NNet" $ eval $ createRandomM @IO @NNet
        , bench "Generate input and runForwards on NNet" $
          eval genAndRunForwards
        , bench "Train NNet on MNIST" $ eval trainNNet
        , bench "Convolution: Generate input and runForwards" $ eval runConv
        , bench "Convolution: runBackwards" $ eval runConvBack
        ]

type Xdim = 28

type Ydim = 28

type ImageShape = 'D2 Xdim Ydim

type Image = S ImageShape

type I = Xdim * Ydim

type IShape = 'D1 I

type H = 30

type HShape = 'D1 H

type O = 10

type OShape = 'D1 O

type FCL = FullyConnected I O

type MyConv = Convolutional 2 3 5 4 10 8

type Inpt = S ('D3 100 100 2)

type Outpt = S ('D3 19 49 3)

type NNet
     = Network '[ Reshape, FullyConnected I H, Sigmoid, FullyConnected H O, Sigmoid] '[ ImageShape, IShape, HShape, HShape, OShape, OShape]

type NNetData = DataSet ImageShape OShape

eval :: IO a -> Benchmarkable
eval action =
    nfIO $ do
        result <- action
        seq result $ pure ()

genAndRunForwards :: IO (S OShape)
genAndRunForwards = do
    inpt <- genInputShape
    net <- createRandomM @IO @NNet
    pure . snd $ runForwards net inpt

genInputShape :: IO (S ImageShape)
genInputShape = generate genValid

nOfTrain, nOfVal, nOfTest :: Int
nOfTrain = 100

nOfVal = 10

nOfTest = 10

epochs :: Natural
epochs = 2

trainNNet :: IO (Momentum NNet)
trainNNet = do
    momNet <- createRandomM @IO @(Momentum NNet)
    (trainSet, _, _) <- load nOfTrain nOfVal nOfTest
    pure $ evalState (trainNetwork momNet trainSet epochs) params

runConv :: IO Outpt
runConv = do
    conv <- generate $ genValid @MyConv
    inpt <- generate $ genValid @Inpt
    pure . snd $ runForwards conv inpt

runConvBack :: IO (Gradient MyConv, Inpt)
runConvBack = do
    conv <- generate $ genValid @MyConv
    inpt <- generate $ genValid @Inpt
    grad <- generate $ genValid @Outpt
    pure $ runBackwards conv inpt grad

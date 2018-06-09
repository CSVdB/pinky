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

type NNet
   = Network '[ Reshape, FullyConnected I H, Sigmoid, FullyConnected H O, Sigmoid] '[ ImageShape, IShape, HShape, HShape, OShape, OShape]

type NNetData = DataSet ImageShape OShape

main :: IO ()
main =
  defaultMain
    [ bench "Generate input" $ eval genInputShape
    , bench "Generate NNet" $ eval $ createRandomM @IO @NNet
    , bench "Generate input and runForwards on NNet" $ eval genAndRunForwards
    , bench "Train NNet on MNIST" $ eval trainNNet
    ]

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

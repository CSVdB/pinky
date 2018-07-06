{-# LANGUAGE DataKinds #-}

module CNN.Network
    ( CNN
    , params
    ) where

import CNN.Params
import Pinky

type CNN = Network CNNLayers CNNShapes

type CNNLayers
     = '[ Convolutional 1 5 1 3 5 7, MaxPool 2 2, Sigmoid, Convolutional 5 4 1 3 5 5, MaxPool 2 2, Reshape, Sigmoid, FullyConnected 48 64, Sigmoid, FullyConnected 64 10, Sigmoid]

type CNNShapes
     = '[ 'D2 28 28, 'D3 24 8 5, 'D3 12 4 5, 'D3 12 4 5, 'D3 8 6 4, 'D3 4 3 4, 'D1 48, 'D1 48, 'D1 64, 'D1 64, 'D1 10, 'D1 10]

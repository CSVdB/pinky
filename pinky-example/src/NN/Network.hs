{-# LANGUAGE DataKinds #-}

module NN.Network
    ( NN
    , params
    ) where

import NN.Params
import Pinky

type NN
     = Network '[ Reshape, FullyConnected 784 30, Sigmoid, FullyConnected 30 10, Sigmoid] '[ 'D2 28 28, 'D1 784, 'D1 30, 'D1 30, 'D1 10, 'D1 10]

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pinky.Layers.Sigmoid where

import Import

import Pinky.Core
import Pinky.CreateRandom

data Sigmoid =
    Sigmoid
    deriving (Show, Eq, Generic)

instance CreateRandom Sigmoid where
    createRandom seed = (Sigmoid, seed)

instance UpdateLayer Sigmoid where
    applyGradient _ _ _ = Momentum Sigmoid Sigmoid

instance Validity Sigmoid

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

-- The derivative of the sigmoid function is
-- sigma'(x) = sigma(x) * (1 - sigma(x)).
-- Since we already calculated sigma(x) in runForwards,
-- we can store this in the Tape and
instance Layer Sigmoid i i where
    type Tape Sigmoid i i = S i
    runForwards _ inpt =
        let sigma = mapS sigmoid inpt
         in (sigma, sigma)
    runBackwards _ sigma outpt =
        (Gradient Sigmoid, (<#.>) outpt $ mapS (\s -> s * (1 - s)) sigma)

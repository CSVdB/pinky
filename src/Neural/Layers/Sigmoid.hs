{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Neural.Layers.Sigmoid where

import Import

import Neural.Core
import Neural.CreateRandom

data Sigmoid =
    Sigmoid
    deriving (Show, Eq, Generic)

instance CreateRandom Sigmoid where
    createRandom seed = (Sigmoid, seed)

instance UpdateLayer Sigmoid where
    applyGradient _ _ _ = Sigmoid

instance Validity Sigmoid where
    validate = trivialValidation

sigmoid :: Floating a => a -> a
sigmoid x =
    let eMinX = exp $ -x
     in 1 / (1 + eMinX)

sigmoid' :: Floating a => a -> a
sigmoid' x = sigmoid x * (1 - sigmoid x)

instance Layer Sigmoid i i where
    type Tape Sigmoid i i = ()
    runForwards _ inpt = ((), mapS sigmoid inpt)
    runBackwards _ _ outpt = (Gradient Sigmoid, mapS sigmoid' outpt)

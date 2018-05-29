{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Neural.Core.Layer where

import Import

import Neural.Core.HyperParams
import Neural.Core.Shape
import Neural.CreateRandom

data Momentum x = Momentum
    { parameters :: x
    , momenta :: x
    } deriving (Eq, Show, Generic)

newtype Gradient x = Gradient
    { gradientParams :: x
    } deriving (Eq, Show, Generic)

class CreateRandom x =>
      UpdateLayer x
    where
    applyGradient :: x -> Gradient x -> HyperParams -> x

class UpdateLayer x =>
      Layer x (i :: Shape) (o :: Shape)
    where
    type Tape x i o :: *
    runForwards :: x -> S i -> (Tape x i o, S o)
    runBackwards :: x -> Tape x i o -> S o -> (Gradient x, S i)

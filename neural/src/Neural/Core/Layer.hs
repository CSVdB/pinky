{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Neural.Core.Layer where

import Import

import Neural.Core.HyperParams
import Neural.Core.LinearAlgebra
import Neural.Core.Shape
import Neural.CreateRandom
import Neural.Utils

data Momentum x = Momentum
    { parameters :: !x
    , momenta :: !x
    } deriving (Eq, Show, Generic)

instance Validity x => Validity (Momentum x) where
    validate (Momentum p m) = delve "parameters" p <> delve "momenta" m

instance CreateRandom x => CreateRandom (Momentum x) where
    createRandom seed =
        let (p, seed') = createRandom seed
            (m, seed'') = createRandom seed'
         in (Momentum p m, seed'')

data Gradient x =
    Gradient !x
    deriving (Eq, Show, Generic)

instance Validity x => Validity (Gradient x)

class CreateRandom x =>
      UpdateLayer x
    where
    applyGradient :: Momentum x -> Gradient x -> HyperParams -> Momentum x

class UpdateLayer x =>
      Layer x (i :: Shape) (o :: Shape)
    where
    type Tape x i o :: *
    runForwards :: x -> S i -> (Tape x i o, S o)
    runBackwards :: x -> Tape x i o -> S o -> (Gradient x, S i)

applyMomentum ::
       (Prod Double x x, Min x, Plus x)
    => HyperParams
    -> Gradient x
    -> Momentum x
    -> Momentum x
applyMomentum hp (Gradient xGrad) (Momentum x xMom) =
    let momentum = fracToDouble $ hyperMomentum hp
        rate = posToDouble (hyperRate hp) / posToNum (hyperBatchSize hp)
        reg = posToDouble $ hyperRegulator hp
        newXMom = momentum <#> xMom <-> rate <#> xGrad
        newX = (1 - rate * reg) <#> x <+> newXMom
     in Momentum newX newXMom

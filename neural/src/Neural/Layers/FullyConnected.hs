{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Neural.Layers.FullyConnected where

import Import

import Neural.Core
import Neural.CreateRandom
import Neural.Utils

data FullyConnected (i :: Nat) (o :: Nat) = FullyConnected
    { biases :: V o
    , weights :: M o i
    } deriving (Show, Eq, Generic)

instance (KnownNat i, KnownNat o) => CreateRandom (FullyConnected i o) where
    createRandom seed =
        let (rB, seed1) = createRandom seed
            (rW, seed2) = createRandom seed1
         in (FullyConnected rB rW, seed2)

instance (KnownNat i, KnownNat o) => UpdateLayer (FullyConnected i o) where
    applyGradient (FullyConnected bias weight) (Gradient (FullyConnected gradBias gradWeight)) hp =
        let rate = posToDouble (hyperRate hp) / posToNum (hyperBatchSize hp)
            reg = posToDouble $ hyperRegulator hp
            newBias = (1 - rate * reg) <#> bias <-> rate <#> gradBias
            newWeight = (1 - rate * reg) <#> weight <-> rate <#> gradWeight
         in FullyConnected newBias newWeight

instance (KnownNat i, KnownNat o) => Validity (FullyConnected i o) where
    validate (FullyConnected bias weight) = validate bias <> validate weight

instance (KnownNat i, KnownNat o) =>
         Layer (FullyConnected i o) ('D1 i) ('D1 o) where
    type Tape (FullyConnected i o) ('D1 i) ('D1 o) = V i
    runForwards FullyConnected {..} (S1D v) =
        (v, S1D $ weights <#> v <+> biases)
    -- dCdz is the vector containing the partial derivatives
    -- partial C / partial z_i, where z = weights <#> inputVector <+> biases
    runBackwards FullyConnected {..} vIn (S1D dCdz) =
        let gradBiases = dCdz
            gradWeights = dCdz `outerProd` vIn
            -- Derivatives for the next step
            dCdz' = transpose weights <#> dCdz
         in (Gradient $ FullyConnected gradBiases gradWeights, S1D dCdz')

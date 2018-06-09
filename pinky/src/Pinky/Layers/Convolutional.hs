{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Pinky.Layers.Convolutional where

import Import

import Pinky.Core
import Pinky.CreateRandom
import Pinky.Utils

data Convolutional :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> * where
    Convolutional
        :: ( KnownNat chanIn
           , KnownNat chanOut
           , KnownNat strideX
           , KnownNat strideY
           , KnownNat kernelX
           , KnownNat kernelY
           , KnownNat kernel
           , kernel ~ (chanIn * kernelX * kernelY)
           )
        => !(M kernel chanOut)
        -> Convolutional chanIn chanOut kernelX kernelY strideX strideY
--
--instance (KnownNat i, KnownNat o) => CreateRandom (FullyConnected i o) where
--    createRandom seed =
--        let (rB, seed1) = createRandom seed
--            (rW, seed2) = createRandom seed1
--         in (FullyConnected rB rW, seed2)
--
--instance (KnownNat i, KnownNat o) => UpdateLayer (FullyConnected i o) where
--    applyGradient (Momentum (FullyConnected bias weight) (FullyConnected biasMom weightMom)) (Gradient (FullyConnected gradBias gradWeight)) hp =
--        let Momentum newBias newBiasMom =
--                applyMomentum hp (Gradient gradBias) $ Momentum bias biasMom
--            Momentum newWeight newWeightMom =
--                applyMomentum hp (Gradient gradWeight) $
--                Momentum weight weightMom
--         in Momentum (FullyConnected newBias newWeight) $
--            FullyConnected newBiasMom newWeightMom
--
--instance (KnownNat i, KnownNat o) => Validity (FullyConnected i o) where
--    validate (FullyConnected bias weight) = validate bias <> validate weight
--
--instance (KnownNat i, KnownNat o) =>
--         Layer (FullyConnected i o) ('D1 i) ('D1 o) where
--    type Tape (FullyConnected i o) ('D1 i) ('D1 o) = V i
--    runForwards FullyConnected {..} (S1D v) =
--        (v, S1D $ weights <#> v <+> biases)
--    -- dCdz is the vector containing the partial derivatives
--    -- partial C / partial z_i, where z = weights <#> inputVector <+> biases
--    runBackwards (Momentum (FullyConnected bias weight) (FullyConnected biasMom weightMom)) vIn (S1D dCdz) =
--        let gradBias = dCdz
--            gradWeight = dCdz `outerProd` vIn
--            -- Derivatives for the next step
--            dCdz' = transpose weight <#> dCdz
--         in (Gradient $ FullyConnected gradBias gradWeight, S1D dCdz')

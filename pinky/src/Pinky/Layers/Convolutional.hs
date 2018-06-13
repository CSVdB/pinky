{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pinky.Layers.Convolutional
    ( Convolutional(..)
    ) where

import Import

import Pinky.Core
import Pinky.CreateRandom
import Pinky.Utils

import Data.Massiv.Array (Stencil, compute)
import Data.Massiv.Array.Stencil (Stencil, mapStencil)
import Data.Massiv.Core.Index (IxN)

data Convolutional :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> * where
    Convolutional
        :: ( KnownNat chanIn
           , KnownNat chanOut
           , KnownNat strideX
           , KnownNat strideY
           , KnownNat kernelX
           , KnownNat kernelY
           )
        => !(Stencil (IxN 3) Double Double)
        -> Convolutional chanIn chanOut kernelX kernelY strideX strideY

instance ( KnownNat c
         , KnownNat c'
         , KnownNat s
         , KnownNat s'
         , KnownNat k
         , KnownNat k'
         ) =>
         CreateRandom (Convolutional c c' s s' k k') where
    createRandom seed =
        let (kernel, seed') = undefined
         in (Convolutional kernel, seed')

instance ( KnownNat c
         , KnownNat c'
         , KnownNat s
         , KnownNat s'
         , KnownNat k
         , KnownNat k'
         ) =>
         UpdateLayer (Convolutional c c' s s' k k') where
    applyGradient (Momentum (Convolutional kernel) (Convolutional kernMom)) (Gradient (Convolutional kernGrad)) hp =
        let (Momentum kernel' kernMom') =
                applyMomentum hp (Gradient kernGrad) $ Momentum kernel kernMom
         in Momentum (Convolutional kernel') (Convolutional kernMom')

instance ( KnownNat c
         , KnownNat c'
         , KnownNat s
         , KnownNat s'
         , KnownNat k
         , KnownNat k'
         ) =>
         Validity (Convolutional c c' s s' k k') where
    validate (Convolutional kernel) = delve "kernel" kernel

instance ( KnownNat c
         , KnownNat c'
         , KnownNat s
         , KnownNat s'
         , KnownNat k
         , KnownNat k'
         ) =>
         Layer (Convolutional c c' s s' k k') ('D3 x y k) ('D3 x' y' k') where
    type Tape (Convolutional c c' s s' k k') ('D3 x y k) ('D3 x' y' k') = S ('D3 x y k)
    runForwards (Convolutional kernel) (S3D inpt) =
        let outpt =
                S3D . fromJust . massivToM . compute . mapStencil kernel $
                mToMassive inpt
         in (S3D inpt, outpt)
    runBackwards = undefined

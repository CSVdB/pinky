{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pinky.Layers.Softmax where

import Import

import Pinky.Core
import Pinky.CreateRandom

data Softmax =
    Softmax
    deriving (Show, Eq, Generic)

instance CreateRandom Softmax where
    createRandom seed = (Softmax, seed)

instance UpdateLayer Softmax where
    applyGradient _ _ _ = Momentum Softmax Softmax

instance Validity Softmax

softmax ::
       forall n. KnownNat n
    => V n
    -> V n
softmax v =
    let v' = mapV exp v
        s = v' <#> (konstV 1 :: V n)
     in if isInfinite s
            then fromJust . intToV . maxIndex $ v
            else mapV (/ s) v'

instance KnownNat n => Layer Softmax ('D1 n) ('D1 n) where
    type Tape Softmax ('D1 n) ('D1 n) = S ('D1 n)
    runForwards _ (S1D inpt) =
        let outptS = S1D $ softmax inpt
         in (outptS, outptS)
    runBackwards _ (S1D inpt) (S1D grad) =
        (Gradient Softmax, S1D $ softmax' inpt grad)

softmax' ::
       forall n. KnownNat n
    => V n
    -> V n
    -> V n
softmax' softm grad =
    let m = diag softm <-> softm `outerProd` softm
     in m <#> grad

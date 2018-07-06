{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pinky.Layers.Reshape where

import Import

import Pinky.Core
import Pinky.CreateRandom

data Reshape =
    Reshape
    deriving (Show, Eq, Generic)

instance CreateRandom Reshape where
    createRandom seed = (Reshape, seed)

instance UpdateLayer Reshape where
    applyGradient _ _ _ = Momentum Reshape Reshape

instance Validity Reshape

instance (Reshapeable i o, Reshapeable o i) => Layer Reshape i o where
    type Tape Reshape i o = ()
    runForwards _ inpt = ((), reshape inpt)
    runBackwards _ _ outpt = (Gradient Reshape, reshape outpt)

class Reshapeable (i :: Shape) (o :: Shape) where
    reshape :: S i -> S o

instance Reshapeable i i where
    reshape = id

instance (KnownNat a, KnownNat b, KnownNat c, c ~ (a * b)) =>
         Reshapeable ('D1 c) ('D2 a b) where
    reshape (S1D v) = S2D $ vToM v

instance (KnownNat a, KnownNat b, KnownNat c, c ~ (a * b)) =>
         Reshapeable ('D2 a b) ('D1 c) where
    reshape (S2D m) = S1D $ mToV m

instance ( KnownNat a
         , KnownNat b
         , KnownNat c
         , KnownNat (b * c)
         , d ~ (a * (b * c))
         ) =>
         Reshapeable ('D1 d) ('D3 a b c) where
    reshape (S1D v) = S3D $ vToM v

instance (KnownNat d, KnownNat (a * c), d ~ (a * (b * c))) =>
         Reshapeable ('D3 a b c) ('D1 d) where
    reshape (S3D m) = S1D $ mToV m

instance (KnownNat b, KnownNat c, d ~ (b * c)) =>
         Reshapeable ('D2 a d) ('D3 a b c) where
    reshape (S2D m) = S3D m

instance (KnownNat d, d ~ (b * c)) => Reshapeable ('D3 a b c) ('D2 a d) where
    reshape (S3D m) = S2D m

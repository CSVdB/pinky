{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pinky.Layers.Resize where

import Import

import Pinky.Core
import Pinky.CreateRandom
import Pinky.Utils

data Resize =
    Resize
    deriving (Show, Eq, Generic)

instance Validity Resize

instance CreateRandom Resize where
    createRandom seed = (Resize, seed)

instance UpdateLayer Resize where
    applyGradient _ _ _ = Momentum Resize Resize

instance forall x x' y y'. (KnownNat x', KnownNat y', KnownNat x, KnownNat y) =>
         Layer Resize ('D2 x y) ('D2 x' y') where
    type Tape Resize ('D2 x y) ('D2 x' y') = ()
    runForwards _ (S2D inpt) = ((), S2D $ resizeM inpt)
    runBackwards _ _ (S2D outpt) = (Gradient Resize, S2D $ resizeM outpt)

instance forall x x' y y' z. ( KnownNat x'
                             , KnownNat y'
                             , KnownNat x
                             , KnownNat y
                             , KnownNat (y * z)
                             , KnownNat (y' * z)
         ) =>
         Layer Resize ('D3 x y z) ('D3 x' y' z) where
    type Tape Resize ('D3 x y z) ('D3 x' y' z) = ()
    runForwards _ (S3D inpt) = ((), S3D $ resizeM inpt)
    runBackwards _ _ (S3D outpt) = (Gradient Resize, S3D $ resizeM outpt)

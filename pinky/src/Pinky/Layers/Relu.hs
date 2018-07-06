{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pinky.Layers.Relu where

import Import

import Pinky.Core
import Pinky.Utils

data Relu =
    Relu
    deriving (Show, Eq, Generic)

instance CreateRandom Relu where
    createRandom seed = (Relu, seed)

instance UpdateLayer Relu where
    applyGradient x _ _ = x

instance Validity Relu

instance Layer Relu i i where
    type Tape Relu i i = ()
    runForwards _ inpt =
        let relued = mapS relu inpt
         in ((), relued)
      where
        relu x =
            if x >= 0
                then x
                else 0
    runBackwards _ _ outpt = (Gradient Relu, mapS relu' outpt)
      where
        relu' x =
            if x >= 0
                then 1
                else 0

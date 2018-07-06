{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Pinky.Layers.Elu where

import Import

import Pinky.Core
import Pinky.Utils

import System.Random

data Elu = Elu
    { eluParam :: Double
    } deriving (Show, Eq, Generic)

instance CreateRandom Double

instance CreateRandom Elu where
    createRandom seed =
        let (x, seed') = createRandom seed
         in (Elu x, seed')

instance UpdateLayer Elu where
    applyGradient x _ _ = x

instance Validity Elu

instance Layer Elu i i where
    type Tape Elu i i = S i
    runForwards (Elu alpha) inpt =
        let elued = mapS elu inpt
         in (elued, elued)
      where
        elu x =
            if x >= 0
                then x
                else alpha * (exp x - 1)
    runBackwards (Elu alpha) elued outpt =
        (Gradient $ Elu alpha, (<#.>) outpt $ mapS elu' elued)
      where
        elu' elued =
            if elued >= 0
            -- This function turns elu(x) into elu'(x)
                then 1
                else elued + alpha

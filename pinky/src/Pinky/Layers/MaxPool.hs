{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Pinky.Layers.MaxPool
    ( MaxPool(..)
    , maxPoolingStencilAccumFunc
    ) where

import Import hiding (traverse)

import Pinky.Core
import Pinky.CreateRandom
import Pinky.Layers.MaxPool.Internal
import Pinky.Layers.Reshape
import Pinky.Utils

import Data.Massiv.Array
    ( Array
    , Comp(..)
    , D
    , DW
    , Size
    , Stencil
    , Value
    , (!)
    , (.+)
    , backpermute
    , compute
    , computeAs
    , delay
    , liftIndex
    , liftIndex2
    , makeArray
    , makeWindowedArray
    , size
    , traverse
    , traverse2
    )
import Data.Massiv.Array.Delayed (D, DW)
import qualified Data.Massiv.Array.Manifest as Manifest
import Data.Massiv.Array.Manifest.Vector (fromVector)
import Data.Massiv.Array.Stencil
    ( makeConvolutionStencilFromKernel
    , makeStencil
    , mapStencil
    , reformDW
    )
import Data.Massiv.Core (Array(..), Load, Manifest, Mutable, Source, iter)
import Data.Massiv.Core.Index
    ( Border(..)
    , Index
    , Ix2(..)
    , Ix3
    , IxN(..)
    , pureIndex
    , zeroIndex
    )

import Data.Default.Class (Default, def)
import Data.List.Extra
import qualified Data.Vector.Storable as SV

import Control.Monad.Random.Class (getRandom)
import Control.Monad.Trans.Random.Lazy (runRand)

import Debug.Trace

import Data.Coerce

data MaxPool :: Nat -> Nat -> * where
    MaxPool :: PoolConstraints a b => MaxPool a b

type PoolConstraints a b = (KnownNat a, KnownNat b)

instance Show (MaxPool a b) where
    show MaxPool = "MaxPool " ++ show (natToInt @a) ++ " " ++ show (natToInt @b)

instance PoolConstraints a b => CreateRandom (MaxPool a b) where
    createRandom seed = (MaxPool, seed)

instance PoolConstraints a b => UpdateLayer (MaxPool a b) where
    applyGradient _ _ _ = Momentum MaxPool MaxPool

instance Validity (MaxPool a b) where
    validate = trivialValidation

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn f x y =
    if f x >= f y
        then x
        else y

getValIx :: (ix -> Value e) -> ix -> Value (e, ix)
getValIx get ix = (, ix) <$> get ix

maxIxAccum :: Ord e => (ix -> Value e) -> ix -> Value (e, ix) -> Value (e, ix)
maxIxAccum get ix currVal = maxOn fst <$> currVal <*> getValIx get ix

{-# INLINE maxPoolingStencilAccumFunc #-}
maxPoolingStencilAccumFunc ::
       (Index ix, Default e, Ord e)
    => ix -- pool size
    -> ((ix -> Value e) -> ix -> Value a -> Value a) -- accumulation functions
    -> ((ix -> Value e) -> ix -> Value a) -- determine initial value
    -> Stencil ix e a
maxPoolingStencilAccumFunc poolSz accumFunc initFunc =
    makeStencil poolSz sCenter getMax
  where
    startIx = zeroIndex
    sCenter = zeroIndex
    getMax get =
        iter startIx poolSz 1 (<) (initFunc get startIx) $ accumFunc get
    {-# INLINE getMax #-}

maxPoolAccumFunc ::
       (Default e, Ord e, Manifest r ix e)
    => ix -- pool size
    -> ((ix -> Value e) -> ix -> Value a -> Value a) -- accumulation functions
    -> ((ix -> Value e) -> ix -> Value a) -- determine initial value
    -> Array r ix e
    -> Array DW ix a
maxPoolAccumFunc poolSz accumFunc initFunc arr =
    let stencil = maxPoolingStencilAccumFunc poolSz accumFunc initFunc
     in reformDW toNewIndex toOldIndex szNew $ mapStencil (Fill def) stencil arr
  where
    sz = size arr
    szNew = liftIndex2 ceil sz poolSz
    toNewIndex ix = liftIndex2 ceil ix poolSz
    toOldIndex = liftIndex2 (*) poolSz

maxPool ::
       (Manifest r ix e, Default e, Ord e)
    => ix -- ^ Size of the maxpooling
    -> Array r ix e -- ^ Array to be maxpooled
    -> Array DW ix (e, ix)
maxPool poolSz = maxPoolAccumFunc poolSz maxIxAccum getValIx

type PoolLayerConstraints a b x y c x' y'
     = ( PoolConstraints a b
       , KnownNat c
       , KnownNat x
       , KnownNat x'
       , KnownNat y
       , KnownNat y'
       , KnownNat (y * c)
       , KnownNat (y' * c)
       , x ~ (x' * a)
       , y ~ (y' * b))

instance (PoolLayerConstraints a b x y 1 x' y') =>
         Layer (MaxPool a b) ('D2 x y) ('D2 x' y') where
    type Tape (MaxPool a b) ('D2 x y) ('D2 x' y') = MaxPoolTape x' y' 1
    runForwards MaxPool inpt =
        let (tape, outpt' :: S ('D3 x' y' 1)) =
                runForwards (MaxPool @a @b) (reshape inpt :: S ('D3 x y 1))
         in (tape, reshape outpt')
    runBackwards MaxPool tape dCdz' =
        let (grad, dCdzR :: S ('D3 x y 1)) =
                runBackwards MaxPool tape (reshape dCdz' :: S ('D3 x' y' 1))
         in (grad, reshape dCdzR)

instance (PoolLayerConstraints a b x y c x' y') =>
         Layer (MaxPool a b) ('D3 x y c) ('D3 x' y' c) where
    type Tape (MaxPool a b) ('D3 x y c) ('D3 x' y' c) = MaxPoolTape x' y' c
    runForwards MaxPool inpt =
        let outptIx = maxPool poolSz $ s3ToMassiv inpt
            outpt = fromJust . massivToS3 . compute $ fst <$> outptIx
         in (MaxPoolTape . compute $ snd <$> outptIx, outpt)
      where
        poolSz = natToInt @a :> natToInt @b :. natToInt @1
    runBackwards MaxPool (MaxPoolTape ixArr) dCdz' =
        let ixAndGradArr = traverse2 outptSz getGradIx ixArr $ s3ToMassiv dCdz'
            predCdz =
                backpermute inptSz (liftIndex2 (flip div) poolSz) ixAndGradArr
            dCdz =
                fromJust . massivToS3 . compute $
                traverse inptSz getGrad predCdz
         in (Gradient MaxPool, dCdz)
      where
        poolSz = natToInt @a :> natToInt @b :. natToInt @1
        outptSz = natToInt @x' :> natToInt @y' :. natToInt @c
        inptSz = natToInt @x :> natToInt @y :. natToInt @c
        getGradIx get1 get2 ix = (get2 ix, get1 ix)
        getGrad get ix =
            let (e, relIx) = get ix
             in if liftIndex2 mod ix poolSz == relIx
                    then e
                    else 0

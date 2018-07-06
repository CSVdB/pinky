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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Pinky.Layers.MaxPooling
    ( MaxPooling(..)
    , maxPool
    , maxPoolingStencilAccumFunc
    ) where

import Import hiding (traverse)

import Pinky.Core
import Pinky.CreateRandom
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

data MaxPooling :: Nat -> Nat -> * where
    MaxPooling :: (KnownNat a, KnownNat b) => MaxPooling a b

instance Show (MaxPooling a b) where
    show MaxPooling =
        "MaxPooling " ++ show (natToInt @a) ++ " " ++ show (natToInt @b)

instance (KnownNat a, KnownNat b) => CreateRandom (MaxPooling a b) where
    createRandom seed = (MaxPooling, seed)

instance (KnownNat a, KnownNat b) => UpdateLayer (MaxPooling a b) where
    applyGradient _ _ _ = Momentum MaxPooling MaxPooling

instance Validity (MaxPooling a b) where
    validate = trivialValidation

maxIx :: Ord e => (e, a) -> (e, a) -> (e, a)
maxIx (v, a) (v', a') =
    if v >= v'
        then (v, a)
        else (v', a')

getValIx :: (ix -> Value e) -> ix -> Value (e, ix)
getValIx get ix = (, ix) <$> get ix

maxIxAccum :: Ord e => (ix -> Value e) -> ix -> Value (e, ix) -> Value (e, ix)
maxIxAccum get ix currVal = maxIx <$> currVal <*> getValIx get ix

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
    startIx = pureIndex 0
    sCenter = pureIndex 0
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
    szNew = liftIndex2 divRoundUp sz poolSz
    toNewIndex ix = liftIndex2 divRoundUp ix poolSz
    toOldIndex = liftIndex2 (*) poolSz

maxPool ::
       (Manifest r ix e, Default e, Ord e)
    => ix -- ^ Size of the maxpooling
    -> Array r ix e -- ^ Array to be maxpooled
    -> Array DW ix e
maxPool poolSz arr =
    maxPoolAccumFunc poolSz (\get ix val -> max <$> val <*> get ix) id arr

maxPoolIx ::
       (Manifest r ix e, Default e, Ord e)
    => ix -- ^ Size of the maxpooling
    -> Array r ix e -- ^ Array to be maxpooled
    -> Array DW ix (e, ix)
maxPoolIx poolSz arr = maxPoolAccumFunc poolSz maxIxAccum getValIx arr

instance ( KnownNat a
         , KnownNat b
         , KnownNat x
         , KnownNat x'
         , KnownNat y
         , KnownNat y'
         , x ~ (x' * a)
         , y ~ (y' * b)
         ) =>
         Layer (MaxPooling a b) ('D2 x y) ('D2 x' y') where
    type Tape (MaxPooling a b) ('D2 x y) ('D2 x' y') = S ('D2 x y)
    runForwards MaxPooling inpt =
        let (_, outpt' :: S ('D3 x' y' 1)) =
                runForwards (MaxPooling @a @b) (reshape inpt :: S ('D3 x y 1))
         in (inpt, reshape outpt')
    runBackwards MaxPooling inpt dCdz' =
        let inptR = reshape inpt :: S ('D3 x y 1)
            dCdzR' = reshape dCdz' :: S ('D3 x' y' 1)
            (grad, dCdzR :: S ('D3 x y 1)) =
                runBackwards MaxPooling inptR dCdzR'
         in (grad, reshape dCdzR)

instance ( KnownNat a
         , KnownNat b
         , KnownNat c
         , KnownNat x
         , KnownNat x'
         , KnownNat y
         , KnownNat y'
         , KnownNat (y * c)
         , KnownNat (y' * c)
         , x ~ (x' * a)
         , y ~ (y' * b)
         ) =>
         Layer (MaxPooling a b) ('D3 x y c) ('D3 x' y' c) where
    type Tape (MaxPooling a b) ('D3 x y c) ('D3 x' y' c) = S ('D3 x y c)
    runForwards MaxPooling inpt =
        let outpt =
                fromJust . massivToS3 . compute . maxPool poolSz $
                s3ToMassiv inpt
         in (inpt, outpt)
      where
        poolSz = natToInt @a :> natToInt @b :. natToInt @1
    runBackwards MaxPooling inpt dCdz' =
        let ixArr = maxPoolIx poolSz $ s3ToMassiv inpt
            ixAndGradArr =
                computeAs Manifest.U $
                (\(_, ix) e -> (e, ix)) <$> delay (computeAs Manifest.U ixArr) <*>
                delay (s3ToMassiv dCdz')
            predCdz =
                backpermute inptSz (liftIndex2 (flip div) poolSz) ixAndGradArr
            dCdz =
                fromJust . massivToS3 . compute $
                traverse inptSz getGrad predCdz
         in (Gradient MaxPooling, dCdz)
      where
        poolSz = natToInt @a :> natToInt @b :. natToInt @1
        inptSz = natToInt @x :> natToInt @y :. natToInt @c
        toDW dArr =
            makeWindowedArray dArr zeroIndex zeroIndex $
            error "The windowed This function should never be called"
        getGrad get ix =
            let (e, relIx) = get ix
             in if liftIndex2 mod ix poolSz == relIx
                    then e
                    else 0

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

module Pinky.Layers.Convolutional
    ( Convolutional(..)
    , genConv
    ) where

import Import

import Pinky.Core
import Pinky.CreateRandom
import Pinky.Layers.Reshape
import Pinky.Utils

import Data.Massiv.Array
    ( Array
    , Comp(..)
    , D
    , DW
    , Stencil
    , computeAs
    , liftIndex
    , liftIndex2
    , size
    )
import Data.Massiv.Array.Delayed (D, DW)
import qualified Data.Massiv.Array.Manifest as Manifest
import Data.Massiv.Array.Manifest.Vector (fromVector)
import Data.Massiv.Array.Stencil
    ( makeConvolutionStencilFromKernel
    , mapStencil
    , reformDW
    )
import Data.Massiv.Core (Array(..), Load, Mutable, Source)
import Data.Massiv.Core.Index (Border(..), Ix2(..), IxN(..), pureIndex)

import qualified Data.Vector.Storable as SV

import Control.Monad.Random.Class (getRandom)
import Control.Monad.Trans.Random.Lazy (runRand)

import Debug.Trace

data Convolutional :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> * where
    Convolutional
        :: ( KnownNat chanIn
           , KnownNat chanOut
           , KnownNat strideX
           , KnownNat strideY
           , KnownNat kernelX
           , KnownNat kernelY
           )
        => !(MyVec chanOut (Array Manifest.S Ix2 Double))
        -> Convolutional chanIn chanOut kernelX kernelY strideX strideY

instance Show (Convolutional c c' s s' k k') where
    show (Convolutional kernels) = "Convolutional\n" ++ show kernels

genConv ::
       forall c c' s s' k k' m.
       ( KnownNat c
       , KnownNat c'
       , KnownNat s
       , KnownNat s'
       , KnownNat k
       , KnownNat k'
       , Monad m
       )
    => Int
    -> Int
    -> m Double
    -> m (Convolutional c c' s s' k k')
genConv kernSize kernSize' genDouble =
    Convolutional . fromJust . mkMyVec <$> replicateM (natToInt @c') genKernel
  where
    genKernel = fromVector Par (kernSize :. kernSize') <$> genVS
    genVS = SV.fromList <$> replicateM (kernSize * kernSize') genDouble

instance ( KnownNat c
         , KnownNat c'
         , KnownNat s
         , KnownNat s'
         , KnownNat k
         , KnownNat k'
         ) =>
         CreateRandom (Convolutional c c' s s' k k') where
    createRandom seed =
        flip runRand seed $ genConv (natToInt @k) (natToInt @k') getRandom

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

instance Validity (Convolutional c c' s s' k k') where
    validate (Convolutional kernels) =
        mconcat
            [ decorate "The kernels are valid" $ foldMap validate kernels
            , foldMap
                  (declare "Kernel has the right size" .
                   (==) (natToInt @k :. natToInt @k') . size)
                  kernels
            ]

type Constraints c c' s s' k k' x x' y y'
     = ( KnownNat c
       , KnownNat c'
       , 1 <= c'
       , KnownNat s
       , KnownNat s'
       , KnownNat k
       , KnownNat k'
       , x ~ (s * x' - s + k)
       , KnownNat x
       , KnownNat x'
       , (y * c) ~ (s' * y' - s' + k')
       , KnownNat y
       , KnownNat y'
       , KnownNat (y * c)
       , KnownNat (y' * c'))

instance (Constraints 1 1 s s' k k' x x' y y') =>
         Layer (Convolutional 1 1 s s' k k') ('D2 x y) ('D2 x' y') where
    type Tape (Convolutional 1 1 s s' k k') ('D2 x y) ('D2 x' y') = S ('D2 x y)
    runForwards conv inpt =
        let inpt' = reshape inpt :: S ('D3 x y 1)
            (_, outpt' :: S ('D3 x' y' 1)) = runForwards conv inpt'
         in (inpt, reshape outpt')
    runBackwards = undefined

instance (Constraints c 1 s s' k k' x x' y y') =>
         Layer (Convolutional c 1 s s' k k') ('D3 x y c) ('D2 x' y') where
    type Tape (Convolutional c 1 s s' k k') ('D3 x y c) ('D2 x' y') = S ('D3 x y c)
    runForwards conv inpt =
        let (_, outpt' :: S ('D3 x' y' 1)) = runForwards conv inpt
         in (inpt, reshape outpt')
    runBackwards = undefined

instance (Constraints 1 c' s s' k k' x x' y y') =>
         Layer (Convolutional 1 c' s s' k k') ('D2 x y) ('D3 x' y' c') where
    type Tape (Convolutional 1 c' s s' k k') ('D2 x y) ('D3 x' y' c') = S ('D2 x y)
    runForwards conv inpt =
        let inpt' = reshape inpt :: S ('D3 x y 1)
         in (inpt, snd $ runForwards conv inpt')
    runBackwards conv inpt dCdz' =
        let inptR = reshape inpt :: S ('D3 x y 1)
            (grad, dCdzR :: S ('D3 x y 1)) = runBackwards conv inptR dCdz'
         in (grad, reshape dCdzR)

conv ::
       forall x y x' y'. (KnownNat x, KnownNat y, KnownNat x', KnownNat y')
    => Ix2 -- ^ stride
    -> Ix2 -- ^ start index
    -> M x y
    -> Array Manifest.S Ix2 Double
    -> M x' y'
conv (strideX :. strideY) startIndex m arr =
    fromJust .
    massivToM . computeAs Manifest.S . reformDW toNewIndex toOldIndex newSize $
    mapStencil (Fill 0) stencil $ mToMassiv m
  where
    toNewIndex ix =
        let (a :. b) = liftIndex2 (-) ix startIndex
         in a `divRoundUp` strideX :. b `divRoundUp` strideY
    toOldIndex (a :. b) = liftIndex2 (+) startIndex $ strideX * a :. strideY * b
    newSize = natToInt @x' :. natToInt @y'
    stencil = makeConvolutionStencilFromKernel arr

backConv ::
       (Manifest r Ix2 e, Num e)
    => Ix2 -- ^ stride of the forwards convolution
    -> Ix2 -- ^ output size
    -> Array r Ix2 e -- ^ output of forwards convolution
    -> Array r Ix2 e -- ^ kernel
    -> Array r Ix2 e -- ^ backwards convoluted array
backConv (s :. s') sz outpt kern =
    makeArray Par sz $ \(a :. b) ->
        sum
            [ (outpt ! i :. j) * (kern ! s * i - a :. s' * j - b)
            | i <- [divRoundUp a s .. min (divRoundUp (k + a) s) x - 1]
            , j <- [divRoundUp b s' .. min (divRoundUp (k' + b) s') y - 1]
            ]
  where
    (k :. k') = size kern
    (x :. y) = size outpt

instance (Constraints c c' s s' k k' x x' y y') =>
         Layer (Convolutional c c' s s' k k') ('D3 x y c) ('D3 x' y' c') where
    type Tape (Convolutional c c' s s' k k') ('D3 x y c) ('D3 x' y' c') = S ('D3 x y c)
    runForwards (Convolutional kernels) (S3D inpt) =
        let outpt = makeChannels $ toChannel inpt <$> kernels
         in (S3D inpt, outpt)
      where
        toChannel :: M x (y * c) -> Array Manifest.S Ix2 Double -> M x' y'
        toChannel m arr =
            fromJust .
            massivToM .
            computeAs Manifest.S . reformDW toNewIndex toOldIndex newSize $
            mapStencil (Fill 0) stencil $ mToMassiv m
          where
            startIndex = liftIndex (`div` 2) $ size arr
            strideX = natToInt @s
            strideY = natToInt @s'
            toNewIndex ix =
                let (a :. b) = liftIndex2 (-) ix startIndex
                 in a `div` strideX :. b `div` strideY
            toOldIndex (a :. b) =
                liftIndex2 (+) startIndex $ strideX * a :. strideY * b
            newSize = natToInt @x' :. natToInt @y'
            stencil = makeConvolutionStencilFromKernel arr
        startIndex = liftIndex (`div` 2) (natToInt @k :. natToInt @k')
    runBackwards (Convolutional kernels) (S3D inpt) dCdz' =
        let dCdzChannels' = mToMassiv <$> splitChannels dCdz'
            dCdzArrayPerChannel (dCdzM', kern) =
                delay $ backConv sSize inptSize dCdzM' kern
            dCdz =
                fromJust . massivToM . compute . foldl1' (.+) $
                dCdzArrayPerChannel <$> zipMyVec dCdzChannels' kernels
            gradChannel z dCdzM' = backConv sSize kSize dCdzM' z
            grad = gradChannel (mToMassiv inpt) <$> dCdzChannels'
         in (Gradient $ Convolutional grad, S3D dCdz)
      where
        inptSize = natToInt @x :. natToInt @(y * c)
        sSize = natToInt @s :. natToInt @s'
        kSize = natToInt @k :. natToInt @k'
        startIndex = liftIndex (\a -> a `div` 2 - a) kSize

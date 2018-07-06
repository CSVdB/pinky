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

module Pinky.Layers.ConvAlt
    ( ConvAlt(..)
    , genConvAlt
    ) where

import Import hiding (foldl1')

import Pinky.Core
import Pinky.Layers.Reshape
import Pinky.Utils

import Data.Massiv.Array
    ( Array
    , Comp(..)
    , D
    , DW
    , (!)
    , compute
    , computeAs
    , delay
    , liftIndex
    , liftIndex2
    , makeArray
    , size
    )
import qualified Data.Massiv.Array.Manifest as Manifest
import Data.Massiv.Array.Manifest.Vector (fromVector)
import Data.Massiv.Array.Stencil
    ( makeConvolutionStencilFromKernel
    , mapStencil
    , reformDW
    )
import Data.Massiv.Core (Array(..), Load, Manifest, Mutable, Source)
import Data.Massiv.Core.Index
    ( Border(..)
    , Ix2(..)
    , Ix3
    , IxN(..)
    , pureIndex
    , totalElem
    )

import qualified Data.Vector.Storable as SV

import Control.Monad.Random.Class (getRandom)
import Control.Monad.Trans.Random.Lazy (runRand)

import Debug.Trace

data ConvAlt :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> * where
    ConvAlt
        :: ConvConstraints c c' s s' k k'
        => !(MyVec c' (Array Manifest.S Ix3 Double)) -- ^ kernels should have size k :> k' :. c
        -> ConvAlt c c' s s' k k'

instance Eq (ConvAlt c c' s s' k k') where
    ConvAlt kern == ConvAlt kern' = kern == kern'

instance Show (ConvAlt c c' s s' k k') where
    show (ConvAlt kernels) = "ConvAlt\n" ++ show kernels

type ConvConstraints chanIn chanOut strideX strideY kernelX kernelY
     = ( KnownNat chanIn
       , KnownNat chanOut
       , KnownNat strideX
       , KnownNat strideY
       , KnownNat kernelX
       , KnownNat kernelY)

genConvAlt ::
       forall c c' s s' k k' m. (ConvConstraints c c' s s' k k', Monad m)
    => Ix3
    -> m Double
    -> m (ConvAlt c c' s s' k k')
genConvAlt kernelSz genDouble =
    ConvAlt . fromJust . mkMyVec <$> replicateM (natToInt @c') genKernel
  where
    genKernel = fromVector Par kernelSz <$> genVS
    genVS = SV.fromList <$> replicateM (totalElem kernelSz) genDouble

instance ConvConstraints c c' s s' k k' =>
         CreateRandom (ConvAlt c c' s s' k k') where
    createRandom seed =
        flip runRand seed $
        genConvAlt (natToInt @k :> natToInt @k' :. natToInt @c) getRandom

instance ConvConstraints c c' s s' k k' =>
         UpdateLayer (ConvAlt c c' s s' k k') where
    applyGradient (Momentum (ConvAlt kernel) (ConvAlt kernMom)) (Gradient (ConvAlt kernGrad)) hp =
        let (Momentum kernel' kernMom') =
                applyMomentum hp (Gradient kernGrad) $ Momentum kernel kernMom
         in Momentum (ConvAlt kernel') (ConvAlt kernMom')

instance Validity (ConvAlt c c' s s' k k') where
    validate (ConvAlt kernels) =
        mconcat
            [ decorate "The kernels are valid" $ foldMap validate kernels
            , foldMap
                  (declare "Kernel has the right size" .
                   (==) (natToInt @k :> natToInt @k' :. natToInt @c) . size)
                  kernels
            ]

type Constraints c c' s s' k k' x x' y y'
     = ( ConvConstraints c c' s s' k k'
       , 1 <= c'
       , x ~ (s * x' - s + k)
       , KnownNat x
       , KnownNat x'
       , y ~ (s' * y' - s' + k')
       , KnownNat y
       , KnownNat y'
       , KnownNat (y * c)
       , KnownNat (y' * c'))

instance Constraints 1 1 s s' k k' x x' y y' =>
         Layer (ConvAlt 1 1 s s' k k') ('D2 x y) ('D2 x' y') where
    type Tape (ConvAlt 1 1 s s' k k') ('D2 x y) ('D2 x' y') = S ('D2 x y)
    runForwards conv inpt =
        let inpt' = reshape inpt :: S ('D3 x y 1)
            (_, outpt' :: S ('D3 x' y' 1)) = runForwards conv inpt'
         in (inpt, reshape outpt')
    runBackwards conv inpt dCdz' =
        let inptR = reshape inpt :: S ('D3 x y 1)
            dCdzR' = reshape dCdz' :: S ('D3 x' y' 1)
            (grad, dCdzR :: S ('D3 x y 1)) = runBackwards conv inptR dCdzR'
         in (grad, reshape dCdzR)

instance Constraints c 1 s s' k k' x x' y y' =>
         Layer (ConvAlt c 1 s s' k k') ('D3 x y c) ('D2 x' y') where
    type Tape (ConvAlt c 1 s s' k k') ('D3 x y c) ('D2 x' y') = S ('D3 x y c)
    runForwards conv inpt =
        let (_, outpt' :: S ('D3 x' y' 1)) = runForwards conv inpt
         in (inpt, reshape outpt')
    runBackwards conv inpt dCdz' =
        let dCdzR' = reshape dCdz' :: S ('D3 x' y' 1)
         in runBackwards conv inpt dCdzR'

instance Constraints 1 c' s s' k k' x x' y y' =>
         Layer (ConvAlt 1 c' s s' k k') ('D2 x y) ('D3 x' y' c') where
    type Tape (ConvAlt 1 c' s s' k k') ('D2 x y) ('D3 x' y' c') = S ('D2 x y)
    runForwards conv inpt =
        let inpt' = reshape inpt :: S ('D3 x y 1)
         in (inpt, snd $ runForwards conv inpt')
    runBackwards conv inpt dCdz' =
        let inptR = reshape inpt :: S ('D3 x y 1)
            (grad, dCdzR :: S ('D3 x y 1)) = runBackwards conv inptR dCdz'
         in (grad, reshape dCdzR)

instance Constraints c c' s s' k k' x x' y y' =>
         Layer (ConvAlt c c' s s' k k') ('D3 x y c) ('D3 x' y' c') where
    type Tape (ConvAlt c c' s s' k k') ('D3 x y c) ('D3 x' y' c') = S ('D3 x y c)
    runForwards (ConvAlt kernels) inpt =
        let inpt' = s3ToMassiv inpt
            channels =
                compute . conv <$> kernels :: MyVec c' (Array Manifest.S Ix3 Double)
            outpt =
                fromJust . massivToS3 . makeArray Par outptSz $ toIx3 channels
         in (inpt, outpt)
      where
        xS' = natToInt @x'
        yS' = natToInt @y'
        outptSz = xS' :> yS' :. natToInt @c'
        (siX :> siY :. siC) =
            liftIndex (`div` 2) $
            natToInt @k - 1 :> natToInt @k' - 1 :. natToInt @c - 1
        strideX = natToInt @s
        strideY = natToInt @s'
        toNewIndex (a :> b :. c) =
            (a - siX) `div` strideX :> (b - siY) `div` strideY :. c - siC
        toOldIndex (a :> b :. c) =
            a * strideX + siX :> b * strideY + siY :. c + siC
        conv kern =
            let arr =
                    mapStencil (Fill 0) (makeConvolutionStencilFromKernel kern) $
                    s3ToMassiv inpt
             in reformDW toNewIndex toOldIndex (xS' :> yS' :. 1) $
                trace ("Stencilled array: " ++ show (computeAs Manifest.U arr)) $
                trace ("Kernel: " ++ show kern) $
                trace ("Input: " ++ show (s3ToMassiv inpt)) $ arr
        toIx3 vec (a :> b :. c) = unsafeMyVecElem vec c ! (a :> b :. 0)
    runBackwards (ConvAlt kernels) (S3D inpt) dCdz' = undefined

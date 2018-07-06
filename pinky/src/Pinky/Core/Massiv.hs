{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Pinky.Core.Massiv where

import Import

import Pinky.Core.LinearAlgebra.Internal
import Pinky.Core.Shape
import Pinky.Utils

import qualified Numeric.LinearAlgebra as NLA
import qualified Numeric.LinearAlgebra.Static as Hmatrix

import Data.Massiv.Array hiding (M, S)
import qualified Data.Massiv.Array.Manifest as Massiv
import Data.Massiv.Array.Manifest.Vector (fromVector, toVector)
import Data.Massiv.Array.Stencil (Stencil(..))
import Data.Massiv.Core (Array, Comp(..))
import Data.Massiv.Core.Index (Index(..), Ix1, Ix2(..), Ix3, IxN(..))

vToMassiv ::
       forall n. KnownNat n
    => V n
    -> Array Massiv.S Ix1 Double
vToMassiv (V v) = fromVector Par (natToInt @n) $ Hmatrix.unwrap v

mToMassiv ::
       forall m n. (KnownNat m, KnownNat n)
    => M m n
    -> Array Massiv.S Ix2 Double
mToMassiv (M m) =
    fromVector Par (natToInt @m :. natToInt @n) . NLA.flatten $ Hmatrix.unwrap m

s3ToMassiv ::
       forall i j k. (KnownNat i, KnownNat j, KnownNat k)
    => S ('D3 i j k)
    -> Array Massiv.S Ix3 Double
s3ToMassiv (S3D (M m)) =
    fromVector Par (natToInt @i :> natToInt @j :. natToInt @k) . NLA.flatten $
    Hmatrix.unwrap m

massivToV :: KnownNat n => Array Massiv.S Ix1 Double -> Maybe (V n)
massivToV = fmap V . Hmatrix.create . toVector

massivToM ::
       forall m n. (KnownNat m, KnownNat n)
    => Array Massiv.S Ix2 Double
    -> Maybe (M m n)
massivToM = fmap M . Hmatrix.create . NLA.reshape (natToInt @n) . toVector

massivToS3 ::
       forall i j k. (KnownNat i, KnownNat j, KnownNat k, KnownNat (j * k))
    => Array Massiv.S Ix3 Double
    -> Maybe (S ('D3 i j k))
massivToS3 =
    fmap (S3D . M) . Hmatrix.create . NLA.reshape (natToInt @(j * k)) . toVector

makeChannels ::
       forall c n m.
       (KnownNat m, KnownNat n, KnownNat c, KnownNat (n * c), 1 <= c)
    => MyVec c (M m n)
    -> S ('D3 m n c)
makeChannels vec = S3D $ mergeMatrices vec

splitChannels ::
       forall c n m.
       (KnownNat m, KnownNat n, KnownNat c, KnownNat (n * c), 1 <= c)
    => S ('D3 m n c)
    -> MyVec c (M m n)
splitChannels (S3D m) = splitMatrices m

flipArr :: Source r ix e => Array r ix e -> Array D ix e
flipArr arr = backpermute sz indexMap arr
  where
    sz = size arr
    indexMap = liftIndex2 (-) $ liftIndex2 (-) sz $ pureIndex 1

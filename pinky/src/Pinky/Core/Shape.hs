{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Pinky.Core.Shape where

import Import

import Pinky.Core.LinearAlgebra
import Pinky.Utils

import qualified Numeric.LinearAlgebra as NLA
import qualified Numeric.LinearAlgebra.Static as Hmatrix

import qualified Data.Massiv.Array.Manifest as Massiv
import Data.Massiv.Array.Manifest.Vector (fromVector, toVector)
import Data.Massiv.Array.Stencil (Stencil(..))
import Data.Massiv.Core (Array, Comp(..))
import Data.Massiv.Core.Index (Index(..), Ix1, Ix2(..), Ix3, IxN(..))

data Shape
    = D1 Nat
    | D2 Nat
         Nat
    | D3 Nat
         Nat
         Nat

-- TODO: Once haskell-src-exts can parse "i * k" in types,
-- change the (*) notation.
data S (s :: Shape) where
    S1D :: KnownNat n => V n -> S ('D1 n)
    S2D :: (KnownNat i, KnownNat j) => M i j -> S ('D2 i j)
    S3D
        :: (KnownNat i, KnownNat j, KnownNat k, KnownNat (j * k))
        => M i (j * k)
        -> S ('D3 i j k)

data instance  Sing (n :: Shape) where
        D1Sing :: Sing a -> Sing ('D1 a)
        D2Sing :: Sing a -> Sing b -> Sing ('D2 a b)
        D3Sing ::
            KnownNat (b * c) => Sing a -> Sing b -> Sing c -> Sing ('D3 a b c)

instance KnownNat a => SingI ('D1 a) where
    sing = D1Sing sing

instance (KnownNat a, KnownNat b) => SingI ('D2 a b) where
    sing = D2Sing sing sing

instance (KnownNat a, KnownNat b, KnownNat c, KnownNat (b * c)) =>
         SingI ('D3 a b c) where
    sing = D3Sing sing sing sing

deriving instance Show (S n)

instance Eq (S n) where
    S1D v == S1D v' = v == v'
    S2D m == S2D m' = m == m'
    S3D m == S3D m' = m == m'

instance Validity (S n) where
    validate (S1D v) = delve "1D shape" v
    validate (S2D m) = delve "2D shape" m
    validate (S3D m) = delve "3D shape" m

mapS :: (Double -> Double) -> S i -> S i
mapS f (S1D v) = S1D $ mapV f v
mapS f (S2D m) = S2D $ mapMatrix f m
mapS f (S3D m) = S3D $ mapMatrix f m

instance Plus (S i) where
    S1D v <+> S1D v' = S1D $ v <+> v'
    S2D m <+> S2D m' = S2D $ m <+> m'
    S3D m <+> S3D m' = S3D $ m <+> m'

instance Min (S i) where
    S1D v <-> S1D v' = S1D $ v <-> v'
    S2D m <-> S2D m' = S2D $ m <-> m'
    S3D m <-> S3D m' = S3D $ m <-> m'

instance ElemProd (S i) where
    S1D v <#.> S1D v' = S1D $ v <#.> v'
    S2D m <#.> S2D m' = S2D $ m <#.> m'
    S3D m <#.> S3D m' = S3D $ m <#.> m'

listToS ::
       forall n. SingI n
    => [Double]
    -> Maybe (S n)
listToS xs =
    case sing :: Sing n of
        D1Sing SNat -> S1D <$> listToV xs
        D2Sing SNat SNat -> S2D <$> listToM xs
        D3Sing SNat SNat SNat -> S3D <$> listToM xs

doubleListToM ::
       forall m n. (KnownNat m, KnownNat n)
    => [[Double]]
    -> Maybe (S ('D2 m n))
doubleListToM xs =
    let m' = natToInt @m
        n' = natToInt @n
     in if m' == length xs && n' == length (head xs)
            then Just . S2D $ unsafeFromDoubleList xs
            else Nothing

trippleListToS ::
       forall i j k. (KnownNat i, KnownNat j, KnownNat k, KnownNat (j * k))
    => [[[Double]]]
    -> Maybe (S ('D3 i j k))
trippleListToS xs =
    if natToInt @i == length xs &&
       natToInt @j == length (head xs) && natToInt @k == length (head (head xs))
        then Just . S3D $ unsafeFromTrippleList xs
        else Nothing

intToS :: KnownNat n => Int -> Maybe (S ('D1 n))
intToS = fmap S1D . intToV

s1ToMassive ::
       forall n. KnownNat n
    => S ('D1 n)
    -> Array Massiv.S Ix1 Double
s1ToMassive (S1D v) = fromVector Par (natToInt @n) $ Hmatrix.unwrap v

s2ToMassive ::
       forall m n. (KnownNat m, KnownNat n)
    => S ('D2 m n)
    -> Array Massiv.S Ix2 Double
s2ToMassive (S2D m) =
    fromVector Par (natToInt @m :. natToInt @n) $ Hmatrix.unwrap m

s3ToMassive ::
       forall i j k. (KnownNat i, KnownNat j, KnownNat k)
    => S ('D3 i j k)
    -> Array Massiv.S Ix3 Double
s3ToMassive (S3D m) =
    fromVector Par (natToInt @i :> natToInt @j :. natToInt @k) $
    Hmatrix.unwrap m

massiveToS1 :: KnownNat n => Array Massiv.S Ix1 Double -> Maybe (S ('D1 n))
massiveToS1 = fmap (S1D . V) . Hmatrix.create . toVector

massiveToS2 ::
       forall m n. (KnownNat m, KnownNat n)
    => Array Massiv.S Ix2 Double
    -> Maybe (S ('D2 m n))
massiveToS2 =
    fmap (S2D . M) . Hmatrix.create . NLA.reshape (natToInt @n) . toVector

massiveToS3 ::
       forall i j k. (KnownNat i, KnownNat j, KnownNat k, KnownNat (j * k))
    => Array Massiv.S Ix3 Double
    -> S ('D3 i j k)
massiveToS3 =
    fmap (S3D . M) . Hmatrix.create . NLA.reshape (natToInt @(j * k)) . toVector

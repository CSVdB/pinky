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
{-# LANGUAGE MultiParamTypeClasses #-}

module Pinky.Core.Shape where

import Import

import Pinky.Core.LinearAlgebra.Internal
import Pinky.Utils

import qualified Numeric.LinearAlgebra as NLA
import qualified Numeric.LinearAlgebra.Static as Hmatrix

data Shape
    = D1 Nat
    | D2 Nat
         Nat
    | D3 Nat
         Nat
         Nat

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

doubleListToS ::
       forall m n. (KnownNat m, KnownNat n)
    => [[Double]]
    -> Maybe (S ('D2 m n))
doubleListToS xs =
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

konstS ::
       forall n. SingI n
    => Double
    -> S n
konstS x =
    case sing :: Sing n of
        D1Sing SNat -> S1D $ konstV x
        D2Sing SNat SNat -> S2D $ konstM x
        D3Sing SNat SNat SNat -> S3D $ konstM x

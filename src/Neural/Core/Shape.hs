{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Neural.Core.Shape where

import Import

import Neural.Core.LinearAlgebra

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
        :: (KnownNat i, KnownNat j, KnownNat k, KnownNat ((*) i k))
        => M ((*) i k) j
        -> S ('D3 i j k)

data instance  Sing (n :: Shape) where
        D1Sing :: Sing a -> Sing ('D1 a)
        D2Sing :: Sing a -> Sing b -> Sing ('D2 a b)
        D3Sing ::
            KnownNat (a * c) => Sing a -> Sing b -> Sing c -> Sing ('D3 a b c)

instance KnownNat a => SingI ('D1 a) where
    sing = D1Sing sing

instance (KnownNat a, KnownNat b) => SingI ('D2 a b) where
    sing = D2Sing sing sing

instance (KnownNat a, KnownNat b, KnownNat c, KnownNat (a * c)) =>
         SingI ('D3 a b c) where
    sing = D3Sing sing sing sing

deriving instance Show (S n)

instance Validity (S (n :: Shape)) where
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

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Neural.Internal.LinearAlgebra where

import Import

import qualified Data.Vector.Storable as SV
import qualified Numeric.LinearAlgebra as NLA
import qualified Numeric.LinearAlgebra.Static as Hmatrix

instance KnownNat n => Eq (Hmatrix.R n) where
    a == b = Hmatrix.extract a == Hmatrix.extract b

instance (KnownNat i, KnownNat j) => Eq (Hmatrix.L i j) where
    a == b = Hmatrix.extract a == Hmatrix.extract b

newtype V (n :: Nat) =
    V (Hmatrix.R n)
    deriving (Show, Eq, Generic)

newtype M (i :: Nat) (j :: Nat) =
    M (Hmatrix.L i j)
    deriving (Show, Eq, Generic)

(<>) ::
       forall m k n. (KnownNat m, KnownNat k, KnownNat n)
    => M m k
    -> M k n
    -> M m n
M a <> M b = M $ a Hmatrix.<> b

infixr 8 <>

(#>) ::
       forall i j. (KnownNat i, KnownNat j)
    => M i j
    -> V j
    -> V i
M m #> V v = V $ m Hmatrix.#> v

instance KnownNat n => Validity (Hmatrix.R n) where
    validate = delve "R-based vector" . SV.toList . Hmatrix.extract

instance KnownNat n => Validity (V n)

instance (KnownNat i, KnownNat j) => Validity (Hmatrix.L i j) where
    validate =
        delve "L-based matrix" . SV.toList . NLA.flatten . Hmatrix.extract

instance (KnownNat i, KnownNat j) => Validity (M i j)

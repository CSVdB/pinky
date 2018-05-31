{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neural.Core.LinearAlgebra.Internal where

import Import

import Neural.CreateRandom
import Neural.Utils.Proxy

import System.Random

import qualified Data.Vector.Storable as SV
import qualified Numeric.LinearAlgebra as NLA
import qualified Numeric.LinearAlgebra.Static as Hmatrix

import Unsafe.Coerce

instance KnownNat n => Eq (Hmatrix.R n) where
    a == b = Hmatrix.extract a == Hmatrix.extract b

instance (KnownNat i, KnownNat j) => Eq (Hmatrix.L i j) where
    a == b = Hmatrix.extract a == Hmatrix.extract b

newtype V (n :: Nat) =
    V (Hmatrix.R n)
    deriving (Show, Eq, Generic)

instance KnownNat n => CreateRandom (V n) where
    createRandom seed =
        let (int, seed') = next seed
            v = V $ Hmatrix.randomVector int NLA.Gaussian
         in (v, seed')

newtype M (i :: Nat) (j :: Nat) =
    M (Hmatrix.L i j)
    deriving (Show, Eq, Generic)

instance (KnownNat m, KnownNat n) => CreateRandom (M m n) where
    createRandom seed =
        let (int, seed') = next seed
            normalisation = 1 / sqrt (fromIntegral $ natToInt @n)
            m =
                M $
                Hmatrix.uniformSample int (Hmatrix.konst (-normalisation)) $
                Hmatrix.konst normalisation
         in (m, seed')

class Prod a b c | a b -> c where
    (<#>) :: a -> b -> c
    infixr 7 <#>

instance KnownNat n => Prod Double (V n) (V n) where
    x <#> V v = unsafeToV $ SV.map (x *) $ Hmatrix.extract v

instance KnownNat n => Prod (V n) Double (V n) where
    v <#> x = mapV (x *) v

instance (KnownNat m, KnownNat n) => Prod (M m n) Double (M m n) where
    m <#> x = mapMatrix (x *) m

instance (KnownNat m, KnownNat n) => Prod Double (M m n) (M m n) where
    x <#> m = mapMatrix (x *) m

instance Prod Double Double Double where
    x <#> y = x * y

instance KnownNat n => Prod (V n) (V n) Double where
    V v <#> V v' = v Hmatrix.<.> v'

instance (KnownNat m, KnownNat k, KnownNat n) =>
         Prod (M m k) (M k n) (M m n) where
    M a <#> M b = M $ a Hmatrix.<> b

instance (KnownNat m, KnownNat n) => Prod (M m n) (V n) (V m) where
    M m <#> V v = V $ m Hmatrix.#> v

unsafeToV :: KnownNat n => SV.Vector Double -> V n
unsafeToV = unsafeCoerce
    --V . fromJust . Hmatrix.create

unsafeToM :: (KnownNat m, KnownNat n) => NLA.Matrix Double -> M m n
unsafeToM = unsafeCoerce
    --M . fromJust . Hmatrix.create

mapV :: KnownNat n => (Double -> Double) -> V n -> V n
mapV f (V v) = V $ Hmatrix.dvmap f v

mapMatrix :: (KnownNat m, KnownNat n) => (Double -> Double) -> M m n -> M m n
mapMatrix f (M m) = M $ Hmatrix.dmmap f m

class Plus a where
    infixl 6 <+>
    (<+>) :: a -> a -> a

instance Plus Double where
    x <+> y = x + y

instance KnownNat n => Plus (V n) where
    V v <+> V v' = V $ v + v'

instance (KnownNat m, KnownNat n) => Plus (M m n) where
    M m <+> M m' = M $ m + m'

class Min a where
    (<->) :: a -> a -> a
    infixl 6 <->

instance Min Double where
    x <-> y = x - y

instance KnownNat n => Min (V n) where
    V v <-> V v' = V $ v - v'

instance (KnownNat m, KnownNat n) => Min (M m n) where
    M m <-> M m' = M $ m - m'

instance KnownNat n => Validity (Hmatrix.R n) where
    validate = delve "R-based vector" . SV.toList . Hmatrix.extract

instance KnownNat n => Validity (V n)

instance (KnownNat i, KnownNat j) => Validity (Hmatrix.L i j) where
    validate =
        delve "L-based matrix" . SV.toList . NLA.flatten . Hmatrix.extract

instance (KnownNat i, KnownNat j) => Validity (M i j)

outerProd :: (KnownNat m, KnownNat n) => V m -> V n -> M m n
outerProd (V v) (V v') = M $ v `Hmatrix.outer` v'

transpose :: (KnownNat m, KnownNat n) => M m n -> M n m
transpose (M m) = M $ Hmatrix.tr m

vToM ::
       forall a b c. (KnownNat a, KnownNat b, KnownNat c, c ~ (a * b))
    => V c
    -> M a b
vToM (V v) = unsafeToM . NLA.reshape (natToInt @b) $ Hmatrix.extract v

mToV ::
       forall a b c. (KnownNat a, KnownNat b, KnownNat c, c ~ (a * b))
    => M a b
    -> V c
mToV (M m) = unsafeToV . NLA.flatten $ Hmatrix.extract m

maxIndex :: KnownNat n => V n -> Int
maxIndex (V v) = SV.maxIndex $ Hmatrix.extract v

unsafeListToV ::
       forall n. KnownNat n
    => [Double]
    -> V n
unsafeListToV = V . Hmatrix.fromList

listToV ::
       forall n. KnownNat n
    => [Double]
    -> Maybe (V n)
listToV xs =
    if length xs == natToInt @n
        then Just $ unsafeListToV xs
        else Nothing

unsafeListToM ::
       forall m n. (KnownNat m, KnownNat n)
    => [Double]
    -> M m n
unsafeListToM = M . Hmatrix.fromList

listToM ::
       forall m n. (KnownNat m, KnownNat n)
    => [Double]
    -> Maybe (M m n)
listToM xs =
    if length xs == natToInt @m * natToInt @n
        then Just $ unsafeListToM xs
        else Nothing

intToV ::
       forall n. KnownNat n
    => Int
    -> Maybe (V n)
intToV x =
    let len = natToInt @n
     in if x < len
            then listToV [oneIfEqual j x | j <- [1 .. len]]
            else Nothing

oneIfEqual :: Int -> Int -> Double
oneIfEqual a b =
    if a == b
        then 1
        else 0

class ElemProd a where
    (<#.>) :: a -> a -> a

instance ElemProd Double where
    (<#.>) = (*)

instance KnownNat n => ElemProd (V n) where
    V v <#.> V v' = V $ v * v'

instance (KnownNat m, KnownNat n) => ElemProd (M m n) where
    M m <#.> M m' = M $ m * m'

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Pinky.Utils.MyVec.Internal where

import Import

import Pinky.Utils.Proxy (natToInt)

newtype MyVec (n :: Nat) a = MyVec
    { myVec :: [a]
    } deriving ( Show
               , Eq
               , Functor
               , Applicative
               , Monad
               , Traversable
               , Foldable
               , Generic
               )

instance Validity a => Validity (MyVec n a)

mkMyVec ::
       forall n a. KnownNat n
    => [a]
    -> Maybe (MyVec n a)
mkMyVec xs =
    if length xs == natToInt @n
        then Just $ MyVec xs
        else Nothing

-- This function is total because of the condition n > 0.
splitFirst :: (KnownNat n, 1 <= n) => MyVec n a -> (a, [a])
splitFirst (MyVec (x:xs)) = (x, xs)
splitFirst (MyVec _) = error "Called splitFirst on an empty MyVec"

foldl1' :: (1 <= n) => (a -> a -> a) -> MyVec n a -> a
foldl1' f (MyVec (x:xs)) = foldl' f x xs
foldl1' _ (MyVec _) = error "Called foldl1' on an empty MyVec"

zipMyVec :: MyVec n a -> MyVec n b -> MyVec n (a, b)
zipMyVec (MyVec as) (MyVec bs) = MyVec $ zip as bs

unsafeMyVecElem :: MyVec n a -> Int -> a
unsafeMyVecElem (MyVec xs) = (!!) xs

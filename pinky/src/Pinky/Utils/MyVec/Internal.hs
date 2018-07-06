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

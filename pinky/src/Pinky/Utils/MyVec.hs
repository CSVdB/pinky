{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Pinky.Utils.MyVec
    ( MyVec
    ) where

import Import

newtype MyVec (n :: Nat) a = MyVec
    { myVec :: [a]
    } deriving (Show, Eq)

instance Functor (MyVec n)

instance Applicative (MyVec n)

instance Monad (MyVec n)

instance Traversable (MyVec n)

instance Foldable (MyVec n)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Pinky.CreateRandom where

import Import

import System.Random

import Control.Monad.Random.Class

class CreateRandom a where
    createRandom :: RandomGen g => g -> (a, g)
    default createRandom :: (Random a, RandomGen g) =>
        g -> (a, g)
    createRandom = random

class MonadCreate m where
    createRandomM :: CreateRandom a => m a

instance MonadRandom m => MonadCreate m where
    createRandomM = do
        stdGen <- mkStdGen <$> getRandom
        pure . fst $ createRandom stdGen

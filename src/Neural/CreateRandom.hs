{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Neural.CreateRandom where

import System.Random

class CreateRandom a where
    createRandom :: RandomGen g => g -> (a, g)
    default createRandom :: (Random a, RandomGen g) =>
        g -> (a, g)
    createRandom = random

{-# OPTIONS_GHC -Wno-orphans #-}

module Neural.Utils.Gen where

import TestImport

import Neural
import Neural.PositiveDouble.Internal

instance GenUnchecked PositiveDouble where
    genUnchecked = PositiveDouble <$> genUnchecked

instance GenValid PositiveDouble where
    genValid = PositiveDouble . abs <$> genValid

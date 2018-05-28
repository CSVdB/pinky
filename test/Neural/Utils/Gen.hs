{-# OPTIONS_GHC -Wno-orphans #-}

module Neural.Utils.Gen where

import TestImport

import Neural
import Neural.Internal.PositiveDouble

instance GenUnchecked PositiveDouble where
    genUnchecked = PositiveDouble <$> genUnchecked

instance GenValid PositiveDouble where
    genValid = PositiveDouble . abs <$> genValid

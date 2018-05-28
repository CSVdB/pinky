{-# OPTIONS_GHC -Wno-orphans #-}

module Neural.Core.Gen where

import TestImport

import Neural
import Neural.Utils.Gen ()

instance GenUnchecked Natural where
    genUnchecked = naturalFromInteger . abs <$> genUnchecked
    shrinkUnchecked n = [1 .. n]

instance GenValid Natural where
    genValid = naturalFromInteger . abs <$> genValid

instance GenUnchecked HyperParams

instance GenValid HyperParams where
    genValid = do
        rate <- genValid
        dr <- genValid
        mom <- genValid
        reg <- genValid
        bs <- genValid
        fromMaybe <$> genValid <*>
            pure (eitherToMaybe $ constructHyperParams rate dr mom reg bs)

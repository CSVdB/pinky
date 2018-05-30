{-# LANGUAGE DeriveGeneric #-}

module Neural.ParamOpt.Accuracy
    ( ClassificationAccuracy(..)
    ) where

import Import

import Neural.Utils

newtype ClassificationAccuracy = ClassificationAccuracy
    { classAcc :: ProperFraction
    } deriving (Show, Eq, Generic, Ord)

instance Validity ClassificationAccuracy

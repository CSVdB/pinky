{-# LANGUAGE DeriveGeneric #-}

module Neural.Utils.PositiveInt.Internal where

import Import

import Data.Aeson (FromJSON, ToJSON)

newtype PositiveInt =
    PositiveInt Natural
    deriving (Show, Eq, Generic, Ord)

instance ToJSON PositiveInt

instance FromJSON PositiveInt

instance Validity PositiveInt where
    validate (PositiveInt n) =
        declare "A PositiveInt is strictly positive" $ n /= 0

constructPositiveInt :: Natural -> Either String PositiveInt
constructPositiveInt = prettyValidation . PositiveInt

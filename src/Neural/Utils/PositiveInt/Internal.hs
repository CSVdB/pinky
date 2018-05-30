{-# LANGUAGE DeriveGeneric #-}

module Neural.Utils.PositiveInt.Internal where

import Import

import Data.Aeson (FromJSON, ToJSON)

import Control.Monad.Catch

newtype PositiveInt =
    PositiveInt Natural
    deriving (Show, Eq, Generic, Ord)

instance ToJSON PositiveInt

instance FromJSON PositiveInt

instance Validity PositiveInt where
    validate (PositiveInt n) =
        declare "A PositiveInt is strictly positive" $ n > 0

newtype NegativePositiveInt =
    NegativePositiveInt String
    deriving (Show, Eq)

instance Exception NegativePositiveInt where
    displayException (NegativePositiveInt errMess) = errMess

constructPositiveInt :: MonadThrow m => Natural -> m PositiveInt
constructPositiveInt n =
    case prettyValidation $ PositiveInt n of
        Left errMess -> throwM $ NegativePositiveInt errMess
        Right y -> pure y

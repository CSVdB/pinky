{-# LANGUAGE DeriveGeneric #-}

module Neural.ProperFraction.Internal where

import Import

newtype ProperFraction =
    ProperFraction Double
    deriving (Show, Eq, Generic)

constructProperFraction :: Double -> Either String ProperFraction
constructProperFraction = prettyValidation . ProperFraction

instance Validity ProperFraction where
    validate (ProperFraction x) =
        mconcat
            [ delve "A ProperFraction contains a valid Double" x
            , declare "A ProperFraction is positive" $ x >= 0
            , declare "A ProperFraction is smaller than 1" $ x <= 1
            ]

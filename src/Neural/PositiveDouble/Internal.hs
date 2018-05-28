{-# LANGUAGE DeriveGeneric #-}

module Neural.PositiveDouble.Internal where

import Import

newtype PositiveDouble =
    PositiveDouble Double
    deriving (Show, Eq, Generic)

constructPositiveDouble :: Double -> Either String PositiveDouble
constructPositiveDouble = prettyValidation . PositiveDouble

instance Validity PositiveDouble where
    validate (PositiveDouble x) =
        mconcat
            [ delve "A PositiveDouble contains a valid Double" x
            , declare "A PositiveDouble is positive" $ x >= 0
            ]

instance Monoid PositiveDouble where
    mempty = PositiveDouble 0
    PositiveDouble x `mappend` PositiveDouble y = PositiveDouble $ x + y

posToDouble :: PositiveDouble -> Double
posToDouble (PositiveDouble x) = x

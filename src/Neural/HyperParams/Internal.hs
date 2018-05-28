{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Neural.HyperParams.Internal where

import Import

import Neural.PositiveDouble
import Neural.ProperFraction

data HyperParams = HyperParams
    { hyperRate :: PositiveDouble
    , hyperDecayRate :: ProperFraction
    , hyperMomentum :: ProperFraction
    , hyperRegulator :: PositiveDouble
    , hyperBatchSize :: Natural
    } deriving (Show, Eq, Generic)

constructHyperParams ::
       PositiveDouble
    -> ProperFraction
    -> ProperFraction
    -> PositiveDouble
    -> Natural
    -> Either String HyperParams
constructHyperParams rate dr mom reg =
    prettyValidation . HyperParams rate dr mom reg

instance Validity HyperParams where
    validate HyperParams {..} =
        let valProd =
                case (< posOne) <$> pMultiply hyperRate hyperRegulator of
                    Left errMess ->
                        invalidOnPos "hyperRate * hyperRegulator" errMess
                    Right True -> valid
                    Right False ->
                        invalidOnPos
                            "hyperRate * hyperRegulator"
                            "Is smaller than 1"
         in valProd <>
            mconcat
                [ delve "hyperRate" hyperRate
                , delve "hyperDecayRate" hyperDecayRate
                , delve "hyperMomentum" hyperMomentum
                , delve "hyperRegulator" hyperRegulator
                , delve "hyperBatchSize" hyperBatchSize
                ]

invalidOnPos :: String -> String -> Validation
invalidOnPos posString errString =
    Validation [Location posString $ Violated errString]

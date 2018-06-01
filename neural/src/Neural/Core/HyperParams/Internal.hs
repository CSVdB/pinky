{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Neural.Core.HyperParams.Internal where

import Import

import Neural.Utils

import Control.Monad.State.Lazy

import Neural.Utils.PositiveInt.Internal

data HyperParams = HyperParams
    { hyperRate :: PositiveDouble
    , hyperDecayRate :: ProperFraction
    , hyperMomentum :: ProperFraction
    , hyperRegulator :: PositiveDouble
    , hyperBatchSize :: PositiveInt
    } deriving (Show, Eq, Generic)

constructHyperParamsFromBasics ::
       Double
    -> Double
    -> Double
    -> Double
    -> Natural
    -> Either String HyperParams
constructHyperParamsFromBasics rate' dr' mom' reg' bs' = do
    rate <- constructPositiveDouble rate'
    dr <- constructProperFraction dr'
    mom <- constructProperFraction mom'
    reg <- constructPositiveDouble reg'
    bs <- constructPositiveInt bs'
    prettyValidation $ HyperParams rate dr mom reg bs

constructHyperParams ::
       PositiveDouble
    -> ProperFraction
    -> ProperFraction
    -> PositiveDouble
    -> Natural
    -> Either String HyperParams
constructHyperParams rate dr mom reg =
    prettyValidation . HyperParams rate dr mom reg . PositiveInt

instance Validity HyperParams where
    validate HyperParams {..} =
        let valProd =
                case (< posOne) <$> pMultiply hyperRate hyperRegulator of
                    Left errMess -> invalidOnPos "rate * regulator" errMess
                    Right True -> valid
                    Right False ->
                        invalidOnPos "rate * regulator" "Is smaller than 1"
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

decay :: MonadState HyperParams m => m ()
decay =
    state $ \hp ->
        ((), hp {hyperRate = dMultiply (hyperRate hp) $ hyperDecayRate hp})

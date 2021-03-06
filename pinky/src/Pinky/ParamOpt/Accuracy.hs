{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pinky.ParamOpt.Accuracy
    ( ClassificationAccuracy(..)
    , accuracy
    , constructAccuracy
    , showAccuracy
    , showAccuracyFromNetwork
    ) where

import Import

import Pinky.Core
import Pinky.ParamOpt.DataSet
import Pinky.Utils

import qualified Data.List.NonEmpty as NEL
import Data.Singletons.Prelude (Head, Last)

newtype ClassificationAccuracy = ClassificationAccuracy
    { classAcc :: ProperFraction
    } deriving (Show, Eq, Generic, Ord)

instance Validity ClassificationAccuracy

constructAccuracy :: Double -> Either String ClassificationAccuracy
constructAccuracy x =
    prettyValidation . ClassificationAccuracy =<< constructProperFraction x

accuracy ::
       (i ~ Head shapes, o ~ Last shapes, SingI o)
    => Network layers shapes
    -> DataSet i o
    -> ClassificationAccuracy
accuracy net dataset =
    let outptAndLabel =
            (\(inpt, label) -> (snd $ runNetwork net inpt, label)) <$> dataset
        nOfCorrectPredictions =
            length . NEL.filter (uncurry samePrediction) $ outptAndLabel
     in case constructAccuracy $
             fromIntegral nOfCorrectPredictions / fromIntegral (length dataset) of
            Left errMess -> error errMess
            Right a -> a

samePrediction ::
       forall n. SingI n
    => S n
    -> S n
    -> Bool
samePrediction (S1D label) (S1D outpt) = maxIndex label == maxIndex outpt
samePrediction _ _ = error "The output layer isn't 1D"

showAccuracy :: String -> ClassificationAccuracy -> String
showAccuracy name (ClassificationAccuracy pf) =
    unwords ["The", name, "accuracy is", show (100 * fracToDouble pf) ++ "%."]

showAccuracyFromNetwork ::
       (i ~ Head shapes, o ~ Last shapes, SingI o)
    => Network layers shapes
    -> String
    -> DataSet i o
    -> String
showAccuracyFromNetwork net name dataset =
    let acc = accuracy net dataset
     in showAccuracy name acc

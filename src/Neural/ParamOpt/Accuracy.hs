{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Neural.ParamOpt.Accuracy
    ( ClassificationAccuracy(..)
    , accuracy
    ) where

import Import

import Neural.Core
import Neural.ParamOpt.DataSet
import Neural.Utils

import qualified Data.List.NonEmpty as NEL
import Data.Singletons.Prelude (Head, Last)

newtype ClassificationAccuracy = ClassificationAccuracy
    { classAcc :: ProperFraction
    } deriving (Show, Eq, Generic, Ord)

instance Validity ClassificationAccuracy

accuracy ::
       (i ~ Head shapes, o ~ ('D1 n), o ~ Last shapes, KnownNat n)
    => Network layers shapes
    -> DataSet i o
    -> ClassificationAccuracy
accuracy net dataset =
    let outptAndLabel =
            (\(inpt, label) -> (snd $ runNetwork net inpt, label)) <$> dataset
        nOfCorrectPredictions =
            length . NEL.filter (uncurry samePrediction) $ outptAndLabel
     in case fmap ClassificationAccuracy . constructProperFraction $
             fromIntegral nOfCorrectPredictions / fromIntegral (length dataset) of
            Left errMess -> error errMess
            Right a -> a

samePrediction :: KnownNat n => S ('D1 n) -> S ('D1 n) -> Bool
samePrediction (S1D label) (S1D outpt) = maxIndex label == maxIndex outpt

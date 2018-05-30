{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Neural.ParamOpt.Train
    ( runIteration
    , trainNetwork
    ) where

import Import

import Neural.Core
import Neural.ParamOpt.DataSet
import Neural.Utils

import Data.Singletons.Prelude (Head, Last)

import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf)

import Control.Monad.State.Lazy

runIteration ::
       (i ~ Head shapes, o ~ Last shapes)
    => Network layers shapes
    -> DataSet i o
    -> State HyperParams (Network layers shapes)
runIteration net0 dataset = do
    hp <- get
    decay
    pure $
        foldl' (trainOnChunk hp) net0 $
        chunksOf (fromIntegral . positiveToNat $ hyperBatchSize hp) $
        NEL.toList dataset
  where
    trainOnChunk hp0 netw chunk =
        let !grads = fmap (uncurry $ getGradientOfNetwork netw) chunk
         in foldl' (\net grad -> applyGradientToNetwork net grad hp0) netw grads

trainNetwork ::
       (i ~ Head shapes, o ~ Last shapes)
    => Network layers shapes
    -> DataSet i o
    -> Natural
    -> State HyperParams (Network layers shapes)
trainNetwork net0 dataset epochs =
    case minusNaturalMaybe epochs 1 of
        Nothing -> pure net0
        Just newEpochs -> do
            !net <- runIteration net0 dataset
            trainNetwork net dataset newEpochs

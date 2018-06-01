{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Neural.ParamOpt.Train
    ( runIteration
    , trainNetwork
    ) where

import Import

import Neural.Core

import Neural.ParamOpt.Accuracy
import Neural.ParamOpt.DataSet
import Neural.Utils

import Data.Singletons.Prelude (Head, Last)

import qualified Data.List.NonEmpty as NEL

import Control.Monad.State.Lazy

import Debug.Trace

import Control.Monad.Random
import System.Random.Shuffle

runIteration ::
       (i ~ Head shapes, o ~ Last shapes, SingI o)
    => Network layers shapes
    -> DataSet i o
    -> State HyperParams (Network layers shapes)
runIteration net0 dataset = do
    hp <- get
    decay
    trace (showAccuracyFromNetwork net0 "train" dataset) $
        pure $
        foldl' (trainOnChunk hp) net0 $
        chunksOf (posToNum $ hyperBatchSize hp) $ NEL.toList dataset
  where
    trainOnChunk hp0 netw chunk =
        let !grads = fmap (uncurry $ getGradientOfNetwork netw) chunk
         in foldl' (\net grad -> applyGradientToNetwork net grad hp0) netw grads

trainNetwork ::
       (i ~ Head shapes, o ~ Last shapes, SingI o)
    => Network layers shapes
    -> DataSet i o
    -> Natural
    -> State HyperParams (Network layers shapes)
trainNetwork net0 dataset epochs =
    case minusNaturalMaybe epochs 1 of
        Nothing -> pure net0
        Just newEpochs -> do
            !net <- runIteration net0 dataset
            let shuffledData =
                    NEL.fromList $
                    evalRand (shuffleM $ NEL.toList dataset) $
                    mkStdGen $ fromIntegral newEpochs
            trainNetwork net shuffledData newEpochs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n
    | n == 0 = error "chunksOf doesn't work for the Int 0"
    | n < 0 = error "chunksOf doesn't work for negative Int"
    | otherwise = chunksOfUnsafe n

chunksOfUnsafe :: Int -> [a] -> [[a]]
chunksOfUnsafe _ [] = [[]]
chunksOfUnsafe n xs =
    let (first, rest) = splitAt n xs
     in first : chunksOfUnsafe n rest

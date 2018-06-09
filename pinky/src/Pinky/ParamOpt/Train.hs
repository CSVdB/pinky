{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Pinky.ParamOpt.Train
    ( runIteration
    , trainNetwork
    ) where

import Import

import Pinky.Core

import Pinky.ParamOpt.Accuracy
import Pinky.ParamOpt.DataSet
import Pinky.Utils

import Data.Singletons.Prelude (Head, Last)

import qualified Data.List.NonEmpty as NEL

import Control.Monad.State.Lazy

--import Debug.Trace
import Control.Monad.Random
import System.Random.Shuffle

runIteration ::
       (i ~ Head shapes, o ~ Last shapes, SingI o)
    => Momentum (Network layers shapes)
    -> DataSet i o
    -> State HyperParams (Momentum (Network layers shapes))
runIteration mom0 dataset = do
    hp <- get
    decay
    -- trace (showAccuracyFromNetwork net0 "train" dataset) $
    pure $
        foldl' (trainOnChunk hp) mom0 $
        chunksOf (posToNum $ hyperBatchSize hp) $ NEL.toList dataset
  where
    trainOnChunk hp0 mom@(Momentum net netMom) chunk =
        let !grads = fmap (uncurry $ getGradientOfNetwork net) chunk
         in foldl'
                (\momentum grad -> applyGradientToNetwork momentum grad hp0)
                mom
                grads

trainNetwork ::
       (i ~ Head shapes, o ~ Last shapes, SingI o)
    => Momentum (Network layers shapes)
    -> DataSet i o
    -> Natural
    -> State HyperParams (Momentum (Network layers shapes))
trainNetwork mom0 dataset epochs =
    case minusNaturalMaybe epochs 1 of
        Nothing -> pure mom0
        Just newEpochs -> do
            !mom <- runIteration mom0 dataset
            let shuffledData =
                    NEL.fromList $
                    evalRand (shuffleM $ NEL.toList dataset) $
                    mkStdGen $ fromIntegral newEpochs
            trainNetwork mom shuffledData newEpochs

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

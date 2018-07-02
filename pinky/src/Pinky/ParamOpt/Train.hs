{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Monad.Trans.Reader

--import Debug.Trace
import Control.Monad.Random
import System.Random.Shuffle

type MyState (o :: Shape) a = StateT HyperParams (Reader (ErrorFunc o)) a

runIteration ::
       forall i o net mom shapes layers.
       ( i ~ Head shapes
       , o ~ Last shapes
       , SingI o
       , net ~ Network layers shapes
       , mom ~ Momentum net
       )
    => Momentum net
    -> DataSet i o
    -> MyState o mom
runIteration mom0 dataset = do
    hp <- get
    decay
    -- trace (showAccuracyFromNetwork net0 "train" dataset) $
    lift $
        foldM (trainOnChunk hp) mom0 $
        chunksOf (posToNum $ hyperBatchSize hp) $ NEL.toList dataset
  where
    applyGrad hp0 m g = applyGradientToNetwork m g hp0
    trainOnChunk ::
           HyperParams -> mom -> [DataPoint i o] -> Reader (ErrorFunc o) mom
    trainOnChunk hp0 momentum@(Momentum net netMom) chunk = do
        !grads <- mapM (uncurry (getGradientOfNetwork net)) chunk
        pure $ foldl' (applyGrad hp0) momentum grads

trainNetwork ::
       ( i ~ Head shapes
       , o ~ Last shapes
       , SingI o
       , net ~ Network layers shapes
       , mom ~ Momentum net
       )
    => Momentum (Network layers shapes)
    -> DataSet i o
    -> Natural
    -> MyState o (Momentum (Network layers shapes))
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

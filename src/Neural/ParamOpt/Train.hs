{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Neural.ParamOpt.Train
    ( runIteration
    ) where

import Import

import Neural.Core
import Neural.ParamOpt.DataSet

import Data.Singletons.Prelude (Head, Last)

import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf)

runIteration ::
       (i ~ Head shapes, o ~ Last shapes)
    => Network layers shapes
    -> DataSet i o
    -> HyperParams
    -> Network layers shapes
runIteration net0 dataset hp =
    foldl' trainOnChunk net0 $
    chunksOf (fromIntegral $ hyperBatchSize hp) $ NEL.toList dataset
  where
    trainOnChunk netw chunk =
        let !grads = flip fmap chunk . uncurry $ getGradientOfNetwork netw
         in foldl' (\net grad -> applyGradientToNetwork net grad hp) netw grads

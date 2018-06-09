{-# LANGUAGE ScopedTypeVariables #-}

module Pinky.Utils.PositiveInt
    ( PositiveInt
    , posToNum
    , constructPositiveInt
    , takePos
    ) where

import Import

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Pinky.Utils.PositiveInt.Internal

takePos :: PositiveInt -> NonEmpty a -> NonEmpty a
takePos (PositiveInt n) = NEL.fromList . NEL.take (fromIntegral n)

posToNum ::
       forall a. Num a
    => PositiveInt
    -> a
posToNum (PositiveInt n) = fromIntegral n

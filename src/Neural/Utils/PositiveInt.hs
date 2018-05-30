module Neural.Utils.PositiveInt
    ( PositiveInt
    , positiveToNat
    , constructPositiveInt
    , takePos
    ) where

import Import

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Neural.Utils.PositiveInt.Internal

positiveToNat :: PositiveInt -> Natural
positiveToNat (PositiveInt n) = n

takePos :: PositiveInt -> NonEmpty a -> NonEmpty a
takePos (PositiveInt n) = NEL.fromList . NEL.take (fromIntegral n)

module Neural.PositiveDouble
    ( PositiveDouble
    , constructPositiveDouble
    , pMultiply
    , posToDouble
    ) where

import Import

import Neural.Internal.PositiveDouble

pMultiply :: PositiveDouble -> PositiveDouble -> Either String PositiveDouble
pMultiply x y = constructPositiveDouble $ posToDouble x * posToDouble y

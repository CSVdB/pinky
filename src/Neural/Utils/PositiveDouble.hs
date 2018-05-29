module Neural.Utils.PositiveDouble
    ( PositiveDouble
    , constructPositiveDouble
    , pMultiply
    , posToDouble
    , posOne
    ) where

import Import

import Neural.Utils.PositiveDouble.Internal

pMultiply :: PositiveDouble -> PositiveDouble -> Either String PositiveDouble
pMultiply x y = constructPositiveDouble $ posToDouble x * posToDouble y

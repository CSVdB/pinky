module Neural.Utils.PositiveDouble
    ( PositiveDouble
    , constructPositiveDouble
    , posToDouble
    , posOne
    , pMultiply
    , dMultiply
    ) where

import Import

import Neural.Utils.PositiveDouble.Internal
import Neural.Utils.ProperFraction

pMultiply :: PositiveDouble -> PositiveDouble -> Either String PositiveDouble
pMultiply x y = constructPositiveDouble $ posToDouble x * posToDouble y

dMultiply :: PositiveDouble -> ProperFraction -> PositiveDouble
dMultiply (PositiveDouble x) pf = PositiveDouble $ x * fracToDouble pf

module Pinky.Utils.PositiveDouble
    ( PositiveDouble
    , constructPositiveDouble
    , posToDouble
    , posOne
    , pMultiply
    , dMultiply
    , pDivide
    ) where

import Import

import Pinky.Utils.PositiveDouble.Internal
import Pinky.Utils.ProperFraction

pMultiply :: PositiveDouble -> PositiveDouble -> Either String PositiveDouble
pMultiply x y = constructPositiveDouble $ posToDouble x * posToDouble y

dMultiply :: PositiveDouble -> ProperFraction -> PositiveDouble
dMultiply (PositiveDouble x) pf = PositiveDouble $ x * fracToDouble pf

pDivide :: PositiveDouble -> PositiveDouble -> PositiveDouble
pDivide (PositiveDouble x) (PositiveDouble y) = PositiveDouble $ x / y

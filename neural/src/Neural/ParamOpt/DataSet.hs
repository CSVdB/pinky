module Neural.ParamOpt.DataSet where

import Neural.Core.Shape

import Data.List.NonEmpty (NonEmpty)

type DataPoint i o = (S i, S o)

type DataSet i o = NonEmpty (DataPoint i o)

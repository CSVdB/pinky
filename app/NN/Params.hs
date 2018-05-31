module NN.Params
    ( params
    ) where

import Neural

params :: HyperParams
params =
    case constructHyperParamsFromBasics 0.3 0.99 0 0 10 of
        Left errMess -> error errMess
        Right x -> x

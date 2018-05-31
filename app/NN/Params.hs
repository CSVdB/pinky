module NN.Params
    ( params
    ) where

import Neural

params :: HyperParams
params =
    case constructHyperParamsFromBasics 8 0.90 0 1e-6 10 of
        Left errMess -> error errMess
        Right x -> x

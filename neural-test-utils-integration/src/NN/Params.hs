module NN.Params
    ( params
    ) where

import Import

import Neural

import GHC.Natural

params :: HyperParams
params = unsafeConstructHP 6 0.50 0 3e-5 10

unsafeConstructHP ::
       Double -> Double -> Double -> Double -> Natural -> HyperParams
unsafeConstructHP a b c d e =
    case constructHyperParamsFromBasics a b c d e of
        Left errMess -> error errMess
        Right x -> x

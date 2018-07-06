module Pinky.Utils.Div where

import Import

divRoundUp :: Int -> Int -> Int
divRoundUp a b = ceiling $ fromIntegral a / fromIntegral b

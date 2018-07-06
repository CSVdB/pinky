module Pinky.Utils.Div where

import Import

ceil :: Int -> Int -> Int
ceil a b = ceiling $ fromIntegral a / fromIntegral b

module Utils.Zip where

import Import

(><) :: [a] -> [b] -> Either String [(a, b)]
(x:xs) >< (y:ys) = fmap ((x, y) :) $ xs >< ys
[] >< [] = Right []
[] >< _ = Left "The lists do not have the same length"
_ >< [] = Left "The lists do not have the same length"

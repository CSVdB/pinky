module Utils.Zip where

import Import

data ZipException =
    ZipException
    deriving (Show, Eq)

instance Exception ZipException where
    displayException ZipException = "The lists do not have the same length"

(><) :: MonadThrow m => [a] -> [b] -> m [(a, b)]
(x:xs) >< (y:ys) = fmap ((x, y) :) $ xs >< ys
[] >< [] = pure []
[] >< _ = throwM ZipException
_ >< [] = throwM ZipException

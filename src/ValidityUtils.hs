module ValidityUtils where

import Prelude

import Data.Validity

delve :: Validity a => String -> a -> Validation
delve = flip annotate

declare :: String -> Bool -> Validation
declare = flip check

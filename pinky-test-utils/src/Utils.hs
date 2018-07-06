{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Prelude

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

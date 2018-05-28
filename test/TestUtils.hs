{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import Data.Proxy
import GHC.TypeLits

natToInt ::
       forall n. KnownNat n
    => Int
natToInt = fromInteger $ natVal $ Proxy @n

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

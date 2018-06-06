{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Prelude

import Data.Typeable

import GHC.TypeLits

natToInt ::
       forall n. KnownNat n
    => Int
natToInt = fromInteger $ natVal $ Proxy @n

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

typeToName ::
       forall a. Typeable a
    => String
typeToName = show . typeRep $ Proxy @a

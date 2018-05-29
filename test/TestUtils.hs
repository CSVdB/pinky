{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module TestUtils where

import Prelude

import Neural

import Data.Proxy
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

type I = 5

type IShape = 'D1 I

type O = 6

type OShape = 'D1 O

type FCL = FullyConnected I O

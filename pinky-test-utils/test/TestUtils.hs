{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TestUtils where

import Prelude

import Pinky

import GHC.TypeLits

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

type Xdim = 5

type Ydim = 3

type ImageShape = 'D2 Xdim Ydim

type Image = S ImageShape

type I = Xdim * Ydim

type IShape = 'D1 I

type H = 8

type HShape = 'D1 H

type O = 6

type OShape = 'D1 O

type FCL = FullyConnected I O

type NNet
     = Network '[ Reshape, FullyConnected I H, Sigmoid, FullyConnected H O, Sigmoid] '[ ImageShape, IShape, HShape, HShape, OShape, OShape]

type NNetTest = Network '[] '[ ImageShape]

type Xpad = 5

type Ypad = 5

type ResizedImageShape = 'D2 (Xdim + Xpad) (Ydim + Ypad)

type ResizedImage = S ResizedImageShape

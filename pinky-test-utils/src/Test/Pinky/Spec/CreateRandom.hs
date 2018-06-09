{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Pinky.Spec.CreateRandom
    ( createRandomSpec
    ) where

import Pinky

import Import
import Test.Hspec
import Test.Pinky.Spec.Gen ()
import Test.Validity

import System.Random

createRandomSpec ::
       forall a. (CreateRandom a, Validity a, Typeable a, Show a)
    => Spec
createRandomSpec =
    describe (unwords ["CreateRandom" ++ typeName]) $
    it (concat ["createRandom :: StdGen -> (", typeName, ", StdGen)"]) $
    forAllValid @StdGen $ \seed -> shouldBeValid $ createRandom @a seed
  where
    typeName = typeToName @a

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Neural.Spec.CreateRandom
    ( createRandomSpec
    ) where

import Neural

import Test.Hspec
import Test.Neural.Spec.Gen ()
import Test.Validity
import TestImport

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

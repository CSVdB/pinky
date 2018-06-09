module Test.Pinky.Core.HyperParamSpec
    ( spec
    ) where

import Test.Hspec
import Test.Pinky.Core.Gen ()
import Test.Validity
import TestImport

import Pinky

import Control.Monad.State.Lazy

spec :: Spec
spec =
    describe "decay :: State HyperParams ()" $
    it "produces valids" $
    forAllValid $ \hp -> shouldBeValid $ evalState decay hp

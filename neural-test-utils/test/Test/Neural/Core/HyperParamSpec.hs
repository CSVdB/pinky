module Test.Neural.Core.HyperParamSpec
    ( spec
    ) where

import Test.Hspec
import Test.Neural.Core.Gen ()
import Test.Validity
import TestImport

import Neural

import Control.Monad.State.Lazy

spec :: Spec
spec =
    describe "decay :: State HyperParams ()" $
    it "produces valids" $
    forAllValid $ \hp -> shouldBeValid $ evalState decay hp

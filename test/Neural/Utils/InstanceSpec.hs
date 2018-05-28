{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Neural.Utils.InstanceSpec where

import Neural
import Neural.Utils.Gen ()

import Test.Hspec
import Test.Validity

spec :: Spec
spec = genValidSpec @PositiveDouble

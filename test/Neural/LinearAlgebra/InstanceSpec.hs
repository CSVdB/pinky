{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Neural.LinearAlgebra.InstanceSpec where

import Neural.LinearAlgebra
import Neural.LinearAlgebra.Gen ()

import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
    genValidSpec @(V 5)
    genValidSpec @(M 5 6)

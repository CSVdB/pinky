{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Pinky.Layers.LayerSpec where

import Test.Hspec
import Test.Pinky.Layers.Gen ()
import Test.Pinky.Spec.Layer
import Test.Validity
import TestImport

import Pinky

spec :: Spec
spec = do
    layerSpec @FCL @IShape @OShape
    layerSpec @Reshape @ImageShape @IShape
    layerSpec @Resize @ResizedImageShape @ImageShape
    layerSpec @Resize @ImageShape @ResizedImageShape
    layerSpec @Sigmoid @ImageShape @ImageShape
    layerSpec @Softmax @OShape @OShape
    layerSpec @NNetTest @ImageShape @ImageShape
    layerSpec @NNet @ImageShape @OShape
    describe "Resize" $ do
        it " doesn't change the structure of the memory" $
            forAllValid @Image $ \inpt ->
                snd (runForwards Resize inpt) `shouldBe` inpt
        it "Resize . Resize == id" $
            forAllValid @Image $ \inpt ->
                let resizedInpt = snd $ runForwards Resize inpt :: ResizedImage
                 in snd (runForwards Resize resizedInpt) `shouldBe` inpt

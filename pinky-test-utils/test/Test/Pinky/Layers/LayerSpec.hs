{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

module Test.Pinky.Layers.LayerSpec where

import Test.Hspec
import Test.Pinky.Layers.Gen ()
import Test.Pinky.Spec.Layer
import Test.QuickCheck
import Test.Validity
import TestImport

import Data.Massiv.Array (Array(..))

import Pinky

type Conv = Convolutional 1 1 3 2 2 4

type Conv2 = Convolutional 2 1 3 2 2 4

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
    describe "Convolutional" $ do
        it "genValid doesn't fail" $ do
            Convolutional !kernels <- generate $ genValid @Conv
            pure ()
        it "isValid doesn't fail" $ do
            conv@(Convolutional !kernels) <- generate $ genValid @Conv
            isValid conv `shouldBe` True
    layerSpec @Conv @('D2 8 8) @('D2 3 3)
    layerSpec @Conv2 @('D3 8 4 2) @('D2 3 3)
    describe "Resize" $ do
        it " doesn't change the structure of the memory" $
            forAllValid @Image $ \inpt ->
                snd (runForwards Resize inpt) `shouldBe` inpt
        it "Resize . Resize == id" $
            forAllValid @Image $ \inpt ->
                let resizedInpt = snd $ runForwards Resize inpt :: ResizedImage
                 in snd (runForwards Resize resizedInpt) `shouldBe` inpt
    layerSpec @Elu @OShape @OShape
    layerSpec @Relu @OShape @OShape

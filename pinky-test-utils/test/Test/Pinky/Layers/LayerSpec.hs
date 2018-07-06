{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Pinky.Layers.LayerSpec where

import Test.Hspec
import Test.Pinky.Layers.Gen ()
import Test.Pinky.Spec.Layer
import Test.QuickCheck
import Test.Validity
import TestImport

import Data.Massiv.Array (Array(..))

import Pinky

import Data.Massiv.Array
    ( Array(..)
    , Border(..)
    , Comp(..)
    , DW
    , Stencil
    , U(..)
    , computeAs
    , delay
    , fromLists'
    , mapStencil
    )
import qualified Data.Massiv.Array.Manifest as Manifest
import Data.Massiv.Core (Index, Ix2(..), Ix3, IxN(..))

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
        it "unit test for runForwards" $
            let inpt =
                    fromJust $ doubleListToS [[8, 7], [6, 5], [4, 3], [2, 1]] :: S ('D2 4 2)
                conv =
                    Convolutional . fromJust $
                    mkMyVec @1 [fromLists' Par [[-1, 0], [0, 1]]] :: Convolutional 1 1 2 2 2 2
                outpt = fromJust $ doubleListToS [[3], [3]] :: S ('D2 2 1)
             in snd (runForwards conv inpt) `shouldBe` outpt
        it "unit test for runBackwards" $
            let inpt =
                    fromJust $ doubleListToS [[8, 7], [6, 5], [4, 3], [2, 1]] :: S ('D2 4 2)
                conv =
                    Convolutional . fromJust $
                    mkMyVec @1 [fromLists' Par [[-1, 0], [0, 1]]] :: Convolutional 1 1 2 2 2 2
                dCdz' = fromJust $ doubleListToS [[-1], [1]] :: S ('D2 2 1)
                dCdz =
                    fromJust $ doubleListToS [[-1, 0], [0, 1], [1, 0], [0, -1]] :: S ('D2 4 2)
                expectedGrad =
                    Gradient . Convolutional . fromJust $
                    mkMyVec @1 [fromLists' Par [[-4, -4], [-4, -4]]]
             in runBackwards conv inpt dCdz' `shouldBe` (expectedGrad, dCdz)
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
    describe "MaxPooling" $ do
        it "mapStencil maxPoolingStencil creates valid output" $
            forAllValid @(S ('D3 4 2 1)) $ \inpt -> do
                let poolSz = 2 :> 2 :. 1
                let !stencil = maxPoolingStencil poolSz
                let mInpt = s3ToMassiv inpt
                let stencilled =
                        mapStencil (Fill 0) stencil mInpt :: Array DW Ix3 Double
                putStrLn "Input:"
                print $ computeAs U $ delay mInpt
                putStrLn "Stencilled:"
                print $ computeAs U stencilled
                shouldBeValid stencilled
        it "unit test for maxPoolingStencil" $
            let arr =
                    fromLists' Par [[1, 2], [3, 4], [5, 6]] :: Array Manifest.S Ix2 Double
                poolSz = 2 :. 2
                stencil = maxPoolingStencil poolSz
                stencilled = mapStencil (Fill 0) stencil arr
             in computeAs U stencilled `shouldBe`
                fromLists' Par [[4, 4], [6, 6], [6, 6]]
>>>>>>> WIP
    layerSpec @(MaxPooling 2 2) @('D2 8 4) @('D2 4 2)
    layerSpec @(MaxPooling 3 2) @('D3 9 4 2) @('D3 3 2 2)
    describe "MaxPooling" $
        it "unit test for runForwards" $
        let inpt =
                fromJust $ doubleListToS [[8, 7], [6, 5], [4, 3], [2, 1]] :: S ('D2 4 2)
            outpt = fromJust $ doubleListToS [[8], [4]] :: S ('D2 2 1)
         in snd (runForwards MaxPooling inpt) `shouldBe` outpt
    describe "MaxPooling" $
        it "unit test for runBackwards" $
        let inpt =
                fromJust $ doubleListToS [[8, 7], [6, 5], [4, 3], [2, 1]] :: S ('D2 4 2)
            dCdz' = fromJust $ doubleListToS [[1], [2]] :: S ('D2 2 1)
            dCdz =
                fromJust $ doubleListToS [[1, 0], [0, 0], [2, 0], [0, 0]] :: S ('D2 4 2)
         in snd (runBackwards MaxPooling inpt dCdz') `shouldBe` dCdz

maxPoolingStencil :: Index ix => ix -> Stencil ix Double Double
maxPoolingStencil poolSz =
    maxPoolingStencilAccumFunc poolSz (\get ix val -> max <$> val <*> get ix) id

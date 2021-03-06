{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Test.Pinky.Core.LinearAlgebraSpec where

import Pinky

import Test.Hspec
import Test.Pinky.Core.Gen ()
import Test.Pinky.Core.LinearAlgebraGen ()
import Test.Validity
import TestImport

type Vect = V 5

type Vect2 = V 6

type Matr = M 5 6

spec :: Spec
spec = do
    genValidSpec @Vect
    genValidSpec @Matr
    plusSpec @Double
    plusSpec @Vect
    plusSpec @Matr
    minSpec @Double
    minSpec @Vect
    minSpec @Matr
    prodSpec @Double @Double @Double
    prodSpec @Double @Vect @Vect
    prodSpec @Vect @Double @Vect
    prodSpec @Double @Matr @Matr
    prodSpec @Matr @Double @Matr
    prodSpec @Matr @Vect2 @Vect
    describe "trippleListToS" $
        it "unit test" $
        let trippleList = replicate 5 (replicate 5 [0, 1])
            shape = trippleListToS trippleList :: Maybe (S ('D3 5 5 2))
            outcome =
                Just $
                S3D $
                unsafeFromDoubleList $ replicate 5 (concat $ replicate 5 [0, 1])
         in shape `shouldBe` outcome
    describe "Massiv" $ do
        it "massivToS1 . s1ToMassiv == Just" $
            equivalentOnValid (massivToS1 . s1ToMassiv @10) Just
        it "massivToS2 . s2ToMassiv == Just" $
            equivalentOnValid (massivToS2 . s2ToMassiv @10 @12) Just
        it "massivToS3 . s3ToMassiv == Just" $
            equivalentOnValid (massivToS3 . s3ToMassiv @10 @12 @14) Just

plusSpec ::
       forall a. (Plus a, GenValid a, Typeable a, Show a)
    => Spec
plusSpec =
    describe (unwords ["(<+>) ::", aName, "->", aName, "->", aName]) $
    it "produces valids on valids" $
    forAllValid @a $ \x -> forAllValid @a $ \y -> shouldBeValid $ x <+> y
  where
    aName = typeToName @a

minSpec ::
       forall a. (Min a, GenValid a, Typeable a, Show a)
    => Spec
minSpec =
    describe (unwords ["(<->) ::", aName, "->", aName, "->", aName]) $
    it "produces valids on valids" $
    forAllValid @a $ \x -> forAllValid @a $ \y -> shouldBeValid $ x <-> y
  where
    aName = typeToName @a

prodSpec ::
       forall a b c.
       ( Prod a b c
       , GenValid a
       , GenValid b
       , Validity c
       , Typeable a
       , Typeable b
       , Typeable c
       , Show a
       , Show b
       , Show c
       )
    => Spec
prodSpec =
    describe (unwords ["(<#>) ::", aName, "->", bName, "->", cName]) $
    it "produces valids on valids" $
    forAllValid @a $ \x -> forAllValid @b $ \y -> shouldBeValid $ x <#> y
  where
    aName = typeToName @a
    bName = typeToName @b
    cName = typeToName @c

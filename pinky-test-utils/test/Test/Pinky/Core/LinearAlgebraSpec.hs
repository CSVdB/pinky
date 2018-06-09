{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Test.Pinky.Core.LinearAlgebraSpec where

import Pinky

import Test.Hspec
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

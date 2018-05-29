{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neural.Core.Network
    ( Network(..)
    , (<:>)
    ) where

import Import

import Neural.Core.HyperParams
import Neural.Core.Layer
import Neural.Core.Shape
import Neural.CreateRandom

import Data.Singletons.Prelude (Head, Last)

data Network :: [*] -> [Shape] -> * where
    EmptyNet :: SingI i => Network '[] '[ i]
    AppendNet
        :: (Layer x i m, SingI i, SingI m)
        => !x
        -> !(Network xs (m ': ss))
        -> Network (x ': xs) (i ': (m ': ss))

(<:>) ::
       (Layer x i m, SingI i, SingI m)
    => x
    -> Network xs (m ': ss)
    -> Network (x ': xs) (i ': (m ': ss))
(<:>) = AppendNet

instance SingI i => Validity (Network '[] '[ i]) where
    validate = trivialValidation

instance (Validity x, Layer x i o, Validity (Network xs (o ': ss))) =>
         Validity (Network (x ': xs) (i ': (o ': ss))) where
    validate (AppendNet x net) = delve "layer in network" x <> validate net

instance SingI i => CreateRandom (Network '[] '[ i]) where
    createRandom seed = (EmptyNet, seed)

instance (Layer x i m, SingI i, SingI m, CreateRandom (Network xs (m ': ss))) =>
         CreateRandom (Network (x ': xs) (i ': (m ': ss))) where
    createRandom seed =
        let (layer, seed1) = createRandom seed
            (net, seed2) = createRandom seed1
         in (AppendNet layer net, seed2)

applyGradientToNetwork ::
       Network layers shapes
    -> Gradient (Network layers shapes)
    -> HyperParams
    -> Network layers shapes
applyGradientToNetwork EmptyNet _ _ = EmptyNet
applyGradientToNetwork (AppendNet layer net) (Gradient (AppendNet gradLayer gradNet)) hp =
    applyGradient layer (Gradient gradLayer) hp <:>
    applyGradientToNetwork net (Gradient gradNet) hp

instance CreateRandom (Network layers shapes) =>
         UpdateLayer (Network layers shapes) where
    applyGradient = applyGradientToNetwork

data Tapes :: [*] -> [Shape] -> * where
    EmptyTape :: SingI i => Tapes '[] '[ i]
    AppendTape
        :: (SingI i, SingI h, Layer x i h)
        => !(Tape x i h)
        -> !(Tapes xs (h ': hs))
        -> Tapes (x ': xs) (i ': h ': hs)

runNetwork ::
       forall layers shapes.
       Network layers shapes
    -> S (Head shapes)
    -> (Tapes layers shapes, S (Last shapes))
runNetwork EmptyNet inpt = (EmptyTape, inpt)
runNetwork (AppendNet layer net) inpt =
    let (tape, inpt') = runForwards layer inpt
        (tapes, outpt) = runNetwork net inpt'
     in (AppendTape tape tapes, outpt)

networkGradient ::
       forall ls ss.
       Network ls ss
    -> Tapes ls ss
    -> S (Last ss)
    -> (Gradient (Network ls ss), S (Head ss))
networkGradient EmptyNet EmptyTape outpt = (Gradient EmptyNet, outpt)
networkGradient (AppendNet layer net) (AppendTape layerTape netTape) outpt =
    let (Gradient gradNet, outpt') = networkGradient net netTape outpt
        (Gradient gradLayer, inpt) = runBackwards layer layerTape outpt'
     in (Gradient (AppendNet gradLayer gradNet), inpt)

instance ( CreateRandom (Network layers shapes)
         , i ~ Head shapes
         , o ~ Last shapes
         ) =>
         Layer (Network layers shapes) i o where
    type Tape (Network layers shapes) i o = Tapes layers shapes
    runForwards = runNetwork
    runBackwards = networkGradient

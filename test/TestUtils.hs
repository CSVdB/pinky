{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import TestImport

natToInt ::
       forall n. KnownNat n
    => Int
natToInt = fromInteger $ natVal $ Proxy @n

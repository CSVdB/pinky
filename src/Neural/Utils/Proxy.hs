{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Neural.Utils.Proxy where

import Import

natToInt ::
       forall n. KnownNat n
    => Int
natToInt = fromInteger . natVal $ Proxy @n

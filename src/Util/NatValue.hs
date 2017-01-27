{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Util.NatValue(natValue) where

import GHC.TypeLits
import Data.Proxy

natValue :: forall n a. (KnownNat n, Num a) => a
natValue = fromIntegral $ natVal (Proxy :: Proxy n)

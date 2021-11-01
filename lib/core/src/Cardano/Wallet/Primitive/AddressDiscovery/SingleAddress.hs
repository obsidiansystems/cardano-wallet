{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- A const value single address

module Cardano.Wallet.Primitive.AddressDiscovery.SingleAddress
    ( SingleAddress (..)
    , mkSingleAddress
    ) where


import Prelude


import Cardano.Wallet.Primitive.AddressDerivation
    ( DerivationIndex (..)
    , NetworkDiscriminant (..)
    , RewardAccount
    )

import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange (..)
    , IsOurs (..)
    )

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )

import Control.DeepSeq
    ( NFData )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy )
import Data.Text.Class
    ( fromText )
import GHC.Generics
    ( Generic )

data SingleAddress (n :: NetworkDiscriminant) = SingleAddress
    { singleAddress :: !Address
        -- ^ The Address
    , derivation :: !(NonEmpty DerivationIndex)
        -- ^ not relevant?
    } deriving (Generic, Show, Eq)

instance NFData (SingleAddress n)

instance IsOurs (SingleAddress n) Address
  where
    isOurs addr state@(SingleAddress addr' d) =
        (if addr == addr' then Just d else Nothing, state)

instance IsOurs (SingleAddress n) RewardAccount
  where
    isOurs _ state = (Nothing, state)

instance GenChange (SingleAddress n) where
    type ArgGenChange (SingleAddress n) = ()
    genChange _ st@(SingleAddress a _) = (a, st)

-- | Since derivation looks irrelevant, add a dummy value for now
mkSingleAddress :: Proxy n -> Address -> SingleAddress n
mkSingleAddress _ a = SingleAddress a (d :| [])
    where Right d = fromText "0H"

-- | This module provides an interface for interacting with the additive signing
-- functions of cardano-wallet.
--
-- Its purpose is to provide a low-level algebra (it works directly on the
-- cardano-api types) that allows signatures to be added to an existing Tx,
-- while maintaining the following important properties:
--
-- 1. Under no circumstances will the algebra modify or destroy existing
-- witnesses of the Tx (sign/witnesses-preserved)
--     ∀tx (f :: Sign era -> Sign era)
--     . getTxWitnesses tx
--       ⊆ getTxWitnesses (toSigned (f (fromSigned tx)))
module Cardano.Wallet.Sign
    (
    -- * Abstract algebra type
    Sign

    -- * Constructors
    , fromSigned
    , fromUnsigned

    -- * Operations
    , addWitness
    , addWitnesses

    -- * Denotation
    , toSigned
    ) where

import Prelude

import Cardano.Api
    ( KeyWitness, Tx (..), TxBody )

data Sign era = Sign (Tx era)

fromSigned :: Tx era -> Sign era
fromSigned = Sign

fromUnsigned :: TxBody era -> Sign era
fromUnsigned body = Sign $ Tx body []

addWitness :: KeyWitness era -> Sign era -> Sign era
addWitness w (Sign (Tx body ws)) = Sign $ Tx body (ws <> [w])

addWitnesses :: [KeyWitness era] -> Sign era -> Sign era
addWitnesses ws' (Sign (Tx body ws)) = Sign $ Tx body (ws <> ws')

toSigned :: Sign era -> Tx era
toSigned (Sign tx) = tx

instance Show (Sign era) where
    show = show . toSigned

instance Eq (Sign era) where
    a == b = toSigned a == toSigned b

-- ∀tx. toSigned (fromSigned tx) = tx
-- ∀ws x. addWitness w x ≠ x
-- ∀ws x. addWitnesses [] x = x
-- ∀ws x. addWitnesses ws x = foldr (.) id (fmap addWitness ws) x
-- ∀tx. fromSigned tx = fromUnsigned (getTxBody tx)
--                      & addWitnesses (getTxWitnesses tx)
-- ∀txBody. fromUnsigned txBody = fromSigned $ Tx txBody []
-- ∀ws' x. toSigned (addWitnesses ws' x) = (\(Tx txBody ws) -> Tx txBody (ws <> ws')) $ toSigned x

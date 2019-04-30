{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains primitives necessary to perform address discovery. So
-- far, we're only interested in address following a sequential derivation
-- scheme as specified in BIP-0044. Later, we may introduce backward
-- compatibility with random address scheme from the legacy Cardano wallets.

module Cardano.Wallet.Primitive.AddressDiscovery
    (
    -- * Sequential Derivation

    -- ** Address Pool Gap
      AddressPoolGap
    , MkAddressPoolGapError (..)
    , defaultAddressPoolGap
    , getAddressPoolGap
    , mkAddressPoolGap

    -- ** Address Pool
    , AddressPool
    , gap
    , addresses
    , changeChain
    , accountPubKey
    , mkAddressPool
    , lookupAddress

    -- * Pending Change Indexes
    , PendingIxs
    , emptyPendingIxs

    -- ** State
    , SeqState (..)
    , mkSeqState
    , AddressScheme (..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv, XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (..)
    , DerivationType (..)
    , Index
    , Key
    , Passphrase (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    , keyToAddress
    , publicKey
    )
import Cardano.Wallet.Primitive.Types
    ( Address, IsOurs (..), invariant )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData, deepseq )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import Text.Read
    ( readMaybe )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                          Sequential Derivation

    Discovery of addresses in the sequential derivation as specified by BIP-44.
    The management of _accounts_ is left-out for this implementation focuses on
    a single account. In practice, one wants to manage a set of pools, one per
    account.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
                              Address Pool Gap
-------------------------------------------------------------------------------}

-- | Maximum number of consecutive undiscovered addresses allowed
newtype AddressPoolGap = AddressPoolGap
    { getAddressPoolGap :: Word8 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData AddressPoolGap

instance FromText AddressPoolGap where
    fromText t = maybe
        (Left err)
        (first (\case ErrGapOutOfRange{} -> err) . mkAddressPoolGap)
        (readMaybe @Word8 (T.unpack t))
      where
        err = TextDecodingError $
            "An address pool gap must be a natural number between "
                <> show (fromEnum $ minBound @AddressPoolGap)
                <> " and "
                <> show (fromEnum $ maxBound @AddressPoolGap)

instance ToText (AddressPoolGap) where
    toText = T.pack . show . getAddressPoolGap

instance Bounded AddressPoolGap where
    minBound = AddressPoolGap 10
    maxBound = AddressPoolGap 100

instance Enum AddressPoolGap where
    fromEnum (AddressPoolGap g) = fromEnum g
    toEnum g
        | AddressPoolGap (toEnum g) < minBound @AddressPoolGap =
            error "AddressPoolGap.toEnum: bad argument"
        | AddressPoolGap (toEnum g) > maxBound @AddressPoolGap =
            error "AddressPoolGap.toEnum: bad argument"
        | otherwise =
            AddressPoolGap (toEnum g)

-- | Smart constructor for 'AddressPoolGap'
mkAddressPoolGap :: Word8 -> Either MkAddressPoolGapError AddressPoolGap
mkAddressPoolGap !g
    | g >= getAddressPoolGap minBound &&
      g <= getAddressPoolGap maxBound = Right $ AddressPoolGap g
    | otherwise = Left $ ErrGapOutOfRange g

-- | Possible errors when casting to an 'AddressPoolGap'
newtype MkAddressPoolGapError = ErrGapOutOfRange Word8
    deriving (Eq, Show)

-- | A default 'AddressPoolGap', as suggested in BIP-0044
defaultAddressPoolGap :: AddressPoolGap
defaultAddressPoolGap =
    AddressPoolGap 20

{-------------------------------------------------------------------------------
                                 Address Pool
-------------------------------------------------------------------------------}

-- | An 'AddressPool' which keeps track of sequential addresses within a given
-- Account and change chain. See 'mkAddressPool' to create a new or existing
-- pool:
--
-- >>> mkAddressPool xpub gap changeChain mempty
-- AddressPool { }
data AddressPool (chain :: ChangeChain) = AddressPool
    { accountPubKey
        :: !(Key 'AccountK XPub)
        -- ^ Corresponding key for the pool (a pool is tied to only one account)
    , gap
        :: !AddressPoolGap
        -- ^ The actual gap for the pool. This can't change for a given pool.
    , indexedAddresses
        :: !(Map Address (Index 'Soft 'AddressK))
    } deriving (Generic, Show, Eq)

instance NFData (AddressPool chain)

-- | Bring a 'ChangeChain' type back to the term-level. This requires a type
-- application and either a scoped type variable, or an explicit passing of a
-- 'ChangeChain'.
--
-- >>> changeChain @'ExternalChain
-- ExternalChain
--
-- >>> changeChain @chain
-- ...
changeChain :: forall (chain :: ChangeChain). Typeable chain => ChangeChain
changeChain =
    case typeRep (Proxy :: Proxy chain) of
        t | t == typeRep (Proxy :: Proxy 'InternalChain) ->
            InternalChain
        _ ->
            ExternalChain

-- | Get all addresses in the pool, sorted from the first address discovered,
-- up until the next one.
--
-- In practice, we always have:
--
-- > mkAddressPool key g cc (addresses pool) == pool
addresses :: AddressPool chain -> [Address]
addresses = map fst . L.sortOn snd . Map.toList . indexedAddresses

-- | Create a new Address pool from a list of addresses. Note that, the list is
-- expected to be ordered in sequence (first indexes, first in the list).
--
-- The pool will grow from the start if less than @g :: AddressPoolGap@ are
-- given, such that, there are always @g@ undiscovered addresses in the pool.
mkAddressPool
    :: forall chain. Typeable chain
    => Key 'AccountK XPub
    -> AddressPoolGap
    -> [Address]
    -> AddressPool chain
mkAddressPool key g addrs = AddressPool
    { accountPubKey = key
    , gap = g
    , indexedAddresses = nextAddresses key g (changeChain @chain) minBound <>
        Map.fromList (zip addrs [minBound..maxBound])
    }


-- | Lookup an address in the pool. When we find an address in a pool, the pool
-- may be amended if the address was discovered near the edge. It is also
-- possible that the pool is not amended at all - this happens in the case that
-- an address is discovered 'far' from the edge.
lookupAddress
    :: Typeable chain
    => Address
    -> AddressPool chain
    -> (Maybe (Index 'Soft 'AddressK), AddressPool chain)
lookupAddress !target !pool =
    case Map.lookup target (indexedAddresses pool) of
        Just ix ->
            (Just ix, extendAddressPool ix pool)
        Nothing ->
            (Nothing, pool)

-- | If an address is discovered near the edge, we extend the address sequence,
-- otherwise we return the pool untouched.
extendAddressPool
    :: forall chain. Typeable chain
    => Index 'Soft 'AddressK
    -> AddressPool chain
    -> AddressPool chain
extendAddressPool !ix !pool
    | isOnEdge  = pool { indexedAddresses = indexedAddresses pool <> next }
    | otherwise = pool
  where
    edge = Map.size (indexedAddresses pool)
    isOnEdge = edge - fromEnum ix <= fromEnum (gap pool)
    next = if ix == maxBound then mempty else nextAddresses
        (accountPubKey pool)
        (gap pool)
        (changeChain @chain)
        (succ ix)

-- | Compute the pool extension from a starting index
nextAddresses
    :: Key 'AccountK XPub
    -> AddressPoolGap
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> Map Address (Index 'Soft 'AddressK)
nextAddresses !key (AddressPoolGap !g) !cc !fromIx =
    [fromIx .. min maxBound toIx]
        & map (\ix -> (newAddress ix, ix))
        & Map.fromList
  where
    toIx = invariant
        "nextAddresses: toIx should be greater than fromIx"
        (toEnum $ fromEnum fromIx + fromEnum g - 1)
        (>= fromIx)
    newAddress = keyToAddress . deriveAddressPublicKey key cc

{-------------------------------------------------------------------------------
                            Pending Change Indexes
-------------------------------------------------------------------------------}

-- | An ordered set of pending indexes. This keep track of indexes used
newtype PendingIxs = PendingIxs [Index 'Soft 'AddressK]
    deriving stock (Generic, Show)
instance NFData PendingIxs

-- | An empty pending set of change indexes.
--
-- NOTE: We do not define a 'Monoid' instance here because there's no rational
-- of combining two pending sets.
emptyPendingIxs :: PendingIxs
emptyPendingIxs = PendingIxs mempty

-- | Update the set of pending indexes by discarding every indexes _below_ the
-- given index.
--
-- Why is that?
--
-- Because we really do care about the higher index that was last used in order
-- to know from where we can generate new indexes.
updatePendingIxs
    :: Index 'Soft 'AddressK
    -> PendingIxs
    -> PendingIxs
updatePendingIxs ix (PendingIxs ixs) =
    PendingIxs $ L.filter (> ix) ixs

-- | Get the next change index; If every available indexes have already been
-- taken, we'll rotate the pending set and re-use already provided indexes.
--
-- This should not be a problem for users in practice, and remain okay for
-- exchanges who care less about privacy / not-reusing addresses than
-- regular users.
nextChangeIndex
    :: AddressPool 'InternalChain
    -> PendingIxs
    -> (Index 'Soft 'AddressK, PendingIxs)
nextChangeIndex pool (PendingIxs ixs) =
    let
        poolLen = length (addresses pool)
        (firstUnused, lastUnused) =
            ( toEnum $ poolLen - fromEnum (gap pool)
            , toEnum $ poolLen - 1
            )
        (ix, ixs') = case ixs of
            [] ->
                (firstUnused, PendingIxs [firstUnused])
            h:_ | length ixs < fromEnum (gap pool) ->
                (succ h, PendingIxs (succ h:ixs))
            h:q ->
                (h, PendingIxs (q++[h]))
    in
        invariant "index is within first unused and last unused" (ix, ixs')
            (\(i,_) -> i >= firstUnused && i <= lastUnused)

{-------------------------------------------------------------------------------
                                 State
-------------------------------------------------------------------------------}

data SeqState = SeqState
    { internalPool :: !(AddressPool 'InternalChain)
        -- ^ Addresses living on the 'InternalChain'
    , externalPool :: !(AddressPool 'ExternalChain)
        -- ^ Addresses living on the 'ExternalChain'
    , pendingChangeIxs :: !PendingIxs
        -- ^ Indexes from the internal pool that have been used in pending
        -- transactions. The list is maintained sorted in descending order
        -- (cf: 'PendingIxs')
    }
    deriving stock (Generic, Show)
instance NFData SeqState

-- | Construct a Sequential state for a wallet.
mkSeqState
    :: (Key 'RootK XPrv, Passphrase "encryption")
    -> AddressPoolGap
    -> SeqState
mkSeqState (rootXPrv, pwd) g =
    let
        accXPrv =
            deriveAccountPrivateKey pwd rootXPrv minBound
        extPool =
            mkAddressPool (publicKey accXPrv) g []
        intPool =
            mkAddressPool (publicKey accXPrv) g []
    in
        SeqState intPool extPool emptyPendingIxs

-- NOTE
-- We have to scan both the internal and external chain. Note that, the
-- account discovery algorithm is only specified for the external chain so
-- in theory, there's nothing forcing a wallet to generate change
-- addresses on the internal chain anywhere in the available range.
instance IsOurs SeqState where
     isOurs addr (SeqState !s1 !s2 !ixs) =
         let
             (internal, !s1') = lookupAddress addr s1
             (external, !s2') = lookupAddress addr s2
             !ixs' = case internal of
                Nothing -> ixs
                Just ix -> updatePendingIxs ix ixs
             ours = isJust (internal <|> external)
         in
             (ixs' `deepseq` ours `deepseq` ours, SeqState s1' s2' ixs')

-- TODO: We might want to move this abstraction / there is more work to be
-- done here.
--
-- It would maybe be nice to derive IsOurs from AddressScheme automatically
-- A /possible/ way to do that would be to return
--     Maybe ((Key 'RootK XPrv, Passphrase "encryption") -> Key 'AddressK XPrv)
-- instead, such that we can use @isJust@ without knowing the rootKey and pwd.
class AddressScheme s where
    keyFrom
        :: Address
        -> (Key 'RootK XPrv, Passphrase "encryption")
        -> s
        -> Maybe (Key 'AddressK XPrv)
        -- ^ Derive the private key corresponding to an address. Careful, this
        -- operation can be costly. Note that the state is discarded from this
        -- function as we do not intend to discover any addresses from this
        -- operation; This is merely a lookup from known addresses.

    nextChangeAddress
        :: s
        -> (Address, s)
        -- ^ Picks the first non-used known change address and use it.
        -- We keep track of pending indexes in the state. In case there's no
        -- more unused change address available, we pick an already existing
        -- one.

instance AddressScheme SeqState where
    keyFrom addr (rootPrv, pwd) (SeqState !s1 !s2 _) =
        let
            xPrv1 = lookupAndDeriveXPrv s1
            xPrv2 = lookupAndDeriveXPrv s2
            xPrv = xPrv1 <|> xPrv2
        in
            xPrv
      where
        lookupAndDeriveXPrv
            :: forall chain. Typeable chain
            => AddressPool chain
            -> Maybe (Key 'AddressK XPrv)
        lookupAndDeriveXPrv pool =
            let
                -- We are assuming there is only one account
                accountPrv = deriveAccountPrivateKey pwd rootPrv minBound
                (addrIx, _) = lookupAddress addr pool
                cc = changeChain @chain
            in
                deriveAddressPrivateKey pwd accountPrv cc <$> addrIx

    -- | We pick indexes in sequence from the first known available index (i.e.
    -- @length addrs - gap@) but we do not generate _new change addresses_. As a
    -- result, we can't generate more than @gap@ _pending_ change addresses and
    -- therefore, rotate the change addresses when we need extra change outputs.
    --
    -- See also: 'nextChangeIndex'
    nextChangeAddress (SeqState intPool extPool pending) =
        let
            (ix, pending') = nextChangeIndex intPool pending
            accountXPub = accountPubKey intPool
            addressXPub = deriveAddressPublicKey accountXPub InternalChain ix
            addr = keyToAddress addressXPub
        in
            (addr, SeqState intPool extPool pending')

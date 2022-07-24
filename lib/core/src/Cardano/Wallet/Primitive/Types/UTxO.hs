{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main 'UTxO' data type used by the wallet.
--
module Cardano.Wallet.Primitive.Types.UTxO
    (
    -- * UTxO
      UTxO (..)

    , dom
    , null
    , size
    , balance
    , isSubsetOf
    , empty
    , disjoint
    , excluding
    , restrictedBy
    , restrictedTo
    , difference
    , partition
    , lookup
    , filter
    , filterByAddressM
    , filterByAddress
    , toList

    -- * UTxO delta encoding
    , DeltaUTxO
    , excluded
    , received
    , excludingD
    , receiveD

    -- * Queries
    , assetIds
    , txIds

    -- * Transformations
    , mapAssetIds
    , mapTxIds
    , removeAssetId

    -- * UTxO Statistics
    , UTxOStatistics (..)
    , BoundType
    , HistogramBar (..)

    , computeStatistics
    , computeUtxoStatistics
    , log10
    ) where

import Prelude hiding
    ( filter, lookup, null )

import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn
    , TxOut (..)
    , txOutAssetIds
    , txOutCoin
    , txOutMapAssetIds
    , txOutRemoveAssetId
    )
import Control.DeepSeq
    ( NFData (..) )
import Data.Bifunctor
    ( bimap, first )
-- import Data.Delta
--     ( Delta (..) )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Set
    ( Set )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), blockListF', blockMapF, padRightF, tupleF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Control.Foldl as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- UTxO
--------------------------------------------------------------------------------

newtype UTxO = UTxO { unUTxO :: Map TxIn TxOut }
    deriving stock (Show, Generic, Eq, Ord)
    deriving newtype (Semigroup, Monoid)

instance NFData UTxO

instance Buildable UTxO where
    build (UTxO utxo) =
        blockListF' "-" utxoF (Map.toList utxo)
      where
        utxoF (inp, out) = buildMap
            [ ("input"
              , build inp)
            , ("output"
              , build out)
            ]
        buildMap = blockMapF . fmap (first $ id @String)

-- | Domain of a 'UTxO' = the set of /inputs/ of the /utxo/.
dom :: UTxO -> Set TxIn
dom (UTxO utxo) = Map.keysSet utxo

-- | Compute the balance of a UTxO
balance :: UTxO -> TokenBundle
balance =
    Map.foldl' fn mempty . unUTxO
  where
    fn :: TokenBundle -> TxOut -> TokenBundle
    fn tot out = tot `TB.add` view #tokens out

difference :: UTxO -> UTxO -> UTxO
difference a b = a `excluding` Map.keysSet (unUTxO b)

-- | Indicates whether a pair of UTxO sets are disjoint.
--
disjoint :: UTxO -> UTxO -> Bool
disjoint u1 u2 = unUTxO u1 `Map.disjoint` unUTxO u2

-- | ins⋪ u
excluding :: UTxO -> Set TxIn ->  UTxO
excluding (UTxO utxo) =
    UTxO . Map.withoutKeys utxo

-- | a ⊆ b
isSubsetOf :: UTxO -> UTxO -> Bool
isSubsetOf (UTxO a) (UTxO b) =
    a `Map.isSubmapOf` b

-- | ins⊲ u
restrictedBy :: UTxO -> Set TxIn -> UTxO
restrictedBy (UTxO utxo) =
    UTxO . Map.restrictKeys utxo

-- | u ⊳ outs
restrictedTo :: UTxO -> Set TxOut -> UTxO
restrictedTo (UTxO utxo) outs =
    UTxO $ Map.filter (`Set.member` outs) utxo

empty :: UTxO
empty = UTxO Map.empty

null :: UTxO -> Bool
null (UTxO u) = Map.null u

size :: UTxO -> Int
size (UTxO u) = Map.size u

-- | Filters a UTxO set according to a condition.
filter :: (TxIn -> Bool) -> UTxO -> UTxO
filter f (UTxO u) = UTxO $ Map.filterWithKey (const . f) u

-- | Lookup an input in the UTXO
lookup :: TxIn -> UTxO -> Maybe TxOut
lookup i (UTxO u) = Map.lookup i u

-- | Filters a 'UTxO' set with an indicator function on 'Address' values.
--
-- Returns the subset of UTxO entries that have addresses for which the given
-- indicator function returns 'True'.
filterByAddressM :: forall f. Monad f => (Address -> f Bool) -> UTxO -> f UTxO
filterByAddressM isOursF (UTxO m) =
    UTxO <$> Map.traverseMaybeWithKey filterFunc m
  where
    filterFunc :: TxIn -> TxOut -> f (Maybe TxOut)
    filterFunc _txin txout = do
        ours <- isOursF $ view #address txout
        pure $ if ours then Just txout else Nothing

-- | Filters a 'UTxO' set with an indicator function on 'Address' values.
--
-- Returns the subset of UTxO entries that have addresses for which the given
-- indicator function returns 'True'.
--
-- filterByAddress f u = runIdentity $ filterByAddressM (pure . f) u
-- filterByAddress (const True) u = u
-- filterByAddress (const False) u = mempty
-- filterByAddress f mempty = mempty
-- filterByAddress f u `isSubsetOf` u
filterByAddress :: (Address -> Bool) -> UTxO -> UTxO
filterByAddress f = runIdentity . filterByAddressM (pure . f)

-- | Partitions a UTxO set according to a condition.
--
-- > filter p a == a && filter (not . p) b == b
-- >   where (a,b) = partition p utxo
partition :: (TxIn -> Bool) -> UTxO -> (UTxO, UTxO)
partition f (UTxO u) = bimap UTxO UTxO $ Map.partitionWithKey (const . f) u

-- | Converts a UTxO set into a list of UTxO elements.
--
toList :: UTxO -> [(TxIn, TxOut)]
toList = Map.toList . unUTxO

{-------------------------------------------------------------------------------
    Delta encodings of UTxO
-------------------------------------------------------------------------------}
-- | Efficient delta encoding for 'UTxO'.
data DeltaUTxO = DeltaUTxO
    { excluded :: !(Set TxIn) -- ^ First exclude these inputs
    , received :: !UTxO       -- ^ Then receive these additional outputs.
    } deriving (Generic, Eq, Show)

-- instance Delta DeltaUTxO where
--     type Base DeltaUTxO = UTxO
--     du `apply` u = (u `excluding` excluded du) <> received du

-- | Left argument is applied /after/ right argument.
instance Semigroup DeltaUTxO where
    db <> da = DeltaUTxO
        { excluded = excluded da <> excluded'db
        , received = received'da <> received db
        }
      where
        received'da = received da `excluding` excluded db
        excluded'db = excluded db `excludingS` received da

-- | Exclude the inputs of a 'UTxO' from a 'Set' of inputs.
excludingS :: Set TxIn -> UTxO -> Set TxIn
excludingS a (UTxO b) = Set.filter (not . (`Map.member` b)) a

-- | Restrict a 'Set' of inputs by the inputs of a 'UTxO'.
restrictedByS :: Set TxIn -> UTxO -> Set TxIn
restrictedByS a (UTxO b) = Set.filter (`Map.member` b) a

instance Monoid DeltaUTxO where
    mempty = DeltaUTxO { excluded = mempty, received = mempty }

-- | Exclude a set of transaction inputs, typically because we spend them.
excludingD :: UTxO -> Set TxIn -> (DeltaUTxO, UTxO)
excludingD u ins = (du, u `excluding` spent)
  where
    spent = ins `restrictedByS` u
    du = DeltaUTxO { excluded = spent, received = mempty }

-- | Receive additional 'UTxO' / union.
receiveD :: UTxO -> UTxO -> (DeltaUTxO, UTxO)
receiveD a b = (da, a <> b)
  where da = DeltaUTxO { excluded = mempty, received = b }

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

assetIds :: UTxO -> Set AssetId
assetIds (UTxO u) = foldMap txOutAssetIds u

txIds :: UTxO -> Set (Hash "Tx")
txIds (UTxO u) = Set.map (view #inputId) (Map.keysSet u)

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

mapAssetIds :: (AssetId -> AssetId) -> UTxO -> UTxO
mapAssetIds f (UTxO u) = UTxO $ Map.map (txOutMapAssetIds f) u

-- | Applies a mapping on transaction identifiers to a 'UTxO' set.
--
-- If the provided mapping gives rise to a collision within the 'TxIn' key set,
-- then only the smallest 'TxOut' is retained, according to the 'Ord' instance
-- for 'TxOut'.
--
mapTxIds :: (Hash "Tx" -> Hash "Tx") -> UTxO -> UTxO
mapTxIds f (UTxO u) = UTxO $ Map.mapKeysWith min (over #inputId f) u

removeAssetId :: UTxO -> AssetId -> UTxO
removeAssetId (UTxO u) a = UTxO $ Map.map (`txOutRemoveAssetId` a) u

--------------------------------------------------------------------------------
-- UTxO Statistics
--------------------------------------------------------------------------------

data UTxOStatistics = UTxOStatistics
    { histogram :: ![HistogramBar]
    , allStakes :: !Word64
    , boundType :: BoundType
    } deriving (Show, Generic, Ord)

instance NFData UTxOStatistics

-- Example output:
--
-- @
--    = Total value of 14061000005 lovelace across 7 UTxOs
--     ... 10                2
--     ... 100               0
--     ... 1000              0
--     ... 10000             0
--     ... 100000            0
--     ... 1000000           0
--     ... 10000000          0
--     ... 100000000         2
--     ... 1000000000        0
--     ... 10000000000       3
--     ... 100000000000      0
--     ... 1000000000000     0
--     ... 10000000000000    0
--     ... 100000000000000   0
--     ... 1000000000000000  0
--     ... 10000000000000000 0
--     ... 45000000000000000 0
--  @
instance Buildable UTxOStatistics where
    build (UTxOStatistics hist val _) = mconcat
        [ "= Total value of "
        , build val
        , " lovelace across "
        , wordF $ sum $ map bucketCount hist
        , " UTxOs"
        , "\n"
        , blockListF' "" buildBar hist
        ]
      where
        buildBar (HistogramBar b c) =
            -- NOTE: Picked to fit well with the max value of Lovelace.
            "... " <> (padRightF 17 ' ' b) <> " " <> wordF c

        -- This is a workaround for the fact that:
        -- > fmt (build (0::Word)) == "-0"
        wordF = build . toInteger

instance Eq UTxOStatistics where
    (UTxOStatistics h s _) == (UTxOStatistics h' s' _) =
        s == s' && sorted h == sorted h'
      where
        sorted :: [HistogramBar] -> [HistogramBar]
        sorted = L.sortOn (\(HistogramBar key _) -> key)

-- An 'HistogramBar' captures the value of a particular bucket. It specifies
-- the bucket upper bound, and its corresponding distribution (on the y-axis).
data HistogramBar = HistogramBar
    { bucketUpperBound :: !Word64
    , bucketCount      :: !Word64
    } deriving (Show, Eq, Ord, Generic)

instance NFData HistogramBar

instance Buildable HistogramBar where
    build (HistogramBar k v) = tupleF (k, v)

--  Buckets boundaries can be constructed in different ways
data BoundType = Log10 deriving (Eq, Show, Ord, Generic)

instance NFData BoundType

-- | Smart-constructor to create bounds using a log-10 scale
log10 :: BoundType
log10 = Log10
{-# INLINE log10 #-}

-- | Compute UtxoStatistics from UTxOs
computeUtxoStatistics :: BoundType -> UTxO -> UTxOStatistics
computeUtxoStatistics btype
    = computeStatistics (pure . Coin.unsafeToWord64 . txOutCoin) btype
    . Map.elems
    . unUTxO

-- | A more generic function for computing UTxO statistics on some other type of
-- data that maps to UTxO's values.
computeStatistics :: (a -> [Word64]) -> BoundType -> [a] -> UTxOStatistics
computeStatistics getCoins btype utxos =
    (F.fold foldStatistics (mconcat $ getCoins <$> utxos)) btype
  where
    foldStatistics :: F.Fold Word64 (BoundType -> UTxOStatistics)
    foldStatistics = UTxOStatistics
        <$> foldBuckets (generateBounds btype)
        <*> F.sum

    foldBuckets :: NonEmpty Word64 -> F.Fold Word64 [HistogramBar]
    foldBuckets bounds =
        let
            step :: Map Word64 Word64 -> Word64 -> Map Word64 Word64
            step x a = case Map.lookupGE a x of
                Just (k, v) -> Map.insert k (v+1) x
                Nothing -> Map.adjust (+1) (NE.head bounds) x
            initial :: Map Word64 Word64
            initial =
                Map.fromList $ zip (NE.toList bounds) (repeat 0)
            extract :: Map Word64 Word64 -> [HistogramBar]
            extract =
                map (uncurry HistogramBar) . Map.toList
        in
            F.Fold step initial extract

    generateBounds :: BoundType -> NonEmpty Word64
    generateBounds = \case
        Log10 -> NE.fromList $ map (10 ^!) [1..16] ++ [45 * (10 ^! 15)]

    (^!) :: Word64 -> Word64 -> Word64
    (^!) = (^)

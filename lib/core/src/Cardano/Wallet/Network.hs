{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)

    -- * Errors
    , ErrPostTx (..)

    -- * Chain following
    , ChainFollower (..)
    , mapChainFollower
    , ChainFollowLog (..)
    , ChainSyncLog (..)
    , mapChainSyncLog
    , withFollowStatsMonitoring

    -- * Logging (for testing)
    , FollowStats (..)
    , History (..)
    , emptyStats
    , updateStats
    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ChainPoint (..)
    , ProtocolParameters
    , SlotNo (..)
    , SlottingParameters (..)
    , StakePoolsSummary
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Control.Monad.Class.MonadSTM
    ( atomically )
import Control.Monad.Class.MonadSTM.Strict
    ( StrictTMVar, newTMVarIO, putTMVar, takeTMVar )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Control.Tracer
    ( Tracer, contramapM, traceWith )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime, diffUTCTime, getCurrentTime )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import NoThunks.Class
    ( AllowThunksIn (..), NoThunks (..) )
import Numeric.Natural
    ( Natural )
import Safe
    ( lastMay )
import UnliftIO.Async
    ( race_ )
import UnliftIO.Concurrent
    ( threadDelay )

import qualified Cardano.Api.Shelley as Node
import qualified Data.List.NonEmpty as NE

data NetworkLayer m block = NetworkLayer
    { chainSync
        :: Tracer IO ChainFollowLog
        -> ChainFollower m
            ChainPoint
            BlockHeader
            block
        -> m ()
        -- ^ Connect to the node and run the ChainSync protocol.
        -- The callbacks provided in the 'ChainFollower' argument
        -- are used to handle intersection finding,
        -- the arrival of new blocks, and rollbacks.

    , currentNodeTip
        :: m BlockHeader
        -- ^ Get the current tip from the chain producer
        --
    , currentNodeEra
        :: m AnyCardanoEra
        -- ^ Get the era the node is currently in.

    , currentProtocolParameters
        :: m ProtocolParameters
        -- ^ Get the last known protocol parameters. In principle, these can
        -- only change once per epoch.

    , currentNodeProtocolParameters
        :: m (Maybe Node.ProtocolParameters)
        -- ^ Get the last known node's protocol parameters. In principle, these can
        -- only change once per epoch.

    , currentSlottingParameters
        :: m SlottingParameters
        -- ^ Get the last known slotting parameters. In principle, these can
        -- only change once per era.

    , watchNodeTip
        :: (BlockHeader -> m ())
        -> m ()
        -- ^ Register a callback for when the node tip changes.
        -- This function should never finish, unless the callback throws an
        -- exception, which will be rethrown by this function.

    , postTx
        :: SealedTx -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , stakeDistribution
        :: Coin -- Stake to consider for rewards
        -> m StakePoolsSummary

    , getCachedRewardAccountBalance
        :: RewardAccount
        -> m Coin
        -- ^ Return the cached reward balance of an account.
        --
        -- If there is no cached value, it will return `Coin 0`, and add the
        -- account to the internal set of observed account, such that it will be
        -- fetched later.

    , fetchRewardAccountBalances
        :: Set RewardAccount
        -> m (Map RewardAccount Coin)
        -- ^ Fetch the reward account balance of a set of accounts without
        -- any caching.

    , timeInterpreter
        :: TimeInterpreter (ExceptT PastHorizonException m)
    , syncProgress
        :: SlotNo -> m (SyncProgress)
    }

instance Functor m => Functor (NetworkLayer m) where
    fmap f nl = nl
        { chainSync = \ tr follower ->
            chainSync nl tr (mapChainFollower id id id f follower)
        }

{-------------------------------------------------------------------------------
                                  Errors
-------------------------------------------------------------------------------}

-- | Error while trying to send a transaction
newtype ErrPostTx = ErrPostTxValidationError Text
    deriving (Generic, Show, Eq)

instance ToText ErrPostTx where
    toText = \case
        ErrPostTxValidationError msg -> msg

{-------------------------------------------------------------------------------
                                Chain Sync
-------------------------------------------------------------------------------}

---- | Subscribe to a blockchain and get called with new block (in order)!
----
---- Exits when the node switches to a different chain with the greatest known
---- common tip between the follower and the node. This makes it easier for client
---- to re-start following from a different point if they have, for instance,
---- rolled back to a point further in the past. If this occurs, clients will need
---- to restart the chain follower from a known list of headers, re-initializing
---- the cursor.
----
---- Exits with 'Nothing' in case of error.
--follow
--    :: forall msg e. (Show e)
--    => NetworkLayer IO
--    -- ^ The @NetworkLayer@ used to poll for new blocks.
--    -> Tracer IO (FollowLog msg)
--    -- ^ Logger trace
--    -> IO [BlockHeader]
--    -- ^ A way to get a list of known tips to start from.
--    -- Blocks /after/ the tip will be yielded.
--    -> (NE.NonEmpty block
--        -> BlockHeader
--        -> Tracer IO msg
--        -> IO (FollowAction e))
--    -- ^ Callback with blocks and the current tip of the /node/.
--    -- @follow@ stops polling and terminates if the callback errors.
--    -> (SlotNo -> IO (Either e SlotNo))
--    -- ^ Callback with blocks and the current tip of the /node/.
--    -- @follow@ stops polling and terminates if the callback errors.
--    -> FollowExceptionRecovery
--    -- ^ Whether to recover from exceptions or not.
--    -> (block -> BlockHeader)
--    -- ^ Getter on the abstract 'block' type
--    -> IO ()
--follow nl tr' readCursor forward' backward recovery header =
--    withFollowStatsMonitoring tr' (syncProgress nl) $ \tr -> do
--        loop tr True
--  where
--    loop tr firstTime = do
--        cursor <- readCursor
--        when firstTime $ traceWith tr $ MsgStartFollowing cursor
--
--        -- Trace the sync progress based on the last "local checkpoint".
--        --
--        -- It appears that @forward@ doesn't get called if we are already
--        -- in-sync. So if we want the @LogState@ to update, we need to trace
--        -- something here.
--        case lastMay cursor of
--            Just c -> traceWith tr . MsgFollowerTip $ Just c
--            Nothing -> traceWith tr . MsgFollowerTip $ Nothing
--
--        let forward blocks tip innerTr = do
--                res <- forward' blocks tip innerTr
--                traceWith tr . MsgFollowerTip . Just $ header $ NE.last blocks
--                return res
--
--        (follow' nl tr cursor forward header) >>= \act -> do
--            case act of
--                FollowFailure ->
--                    -- NOTE: follow' is tracing the error, so we don't have to
--                    -- here
--                    case recovery of
--                        RetryOnExceptions -> loop tr False
--                        AbortOnExceptions -> return ()
--                FollowRollback requestedSlot -> do
--                    -- NOTE: follow' is tracing MsgWillRollback
--                    backward requestedSlot >>= \case
--                        Left e -> do
--                            traceWith tr $ MsgFailedRollingBack $ T.pack (show e)
--                        Right actualSlot -> do
--                            traceWith tr $ MsgDidRollback requestedSlot actualSlot
--                    loop tr False
--                FollowDone ->
--                    -- TODO: Pool used to log MsgHaltMonitoring
--                    return ()
--
---- | A new, more convenient, wrapping @follow@ function was added above.
----
---- This is the old one. It was kept for now to minimise changes and potential
---- mistakes, as it is pretty intricate.
--follow'
--    :: forall block msg e. (Show e)
--    => NetworkLayer IO block
--    -- ^ The @NetworkLayer@ used to poll for new blocks.
--    -> Tracer IO (FollowLog msg)
--    -- ^ Logger trace
--    -> [BlockHeader]
--    -- ^ A list of known tips to start from.
--    -- Blocks /after/ the tip will be yielded.
--    -> (NE.NonEmpty block
--        -> BlockHeader
--        -> Tracer IO msg
--        -> IO (FollowAction e))
--    -- ^ Callback with blocks and the current tip of the /node/.
--    -- @follow@ stops polling and terminates if the callback errors.
--    -> (block -> BlockHeader)
--    -- ^ Getter on the abstract 'block' type
--    -> IO FollowExit
--follow' nl tr cps yield header = error "todo"
--    bracket
--        (initCursor nl cps)
--        (destroyCursor nl)
--        (handleExceptions . sleep 0 False)
--  where
--    innerTr = contramap MsgFollowLog tr
--    delay0 :: Int
--    delay0 = 500*1000 -- 500ms
--
--    handleExceptions :: IO FollowExit -> IO FollowExit
--    handleExceptions =
--        -- Node disconnections are seen as async exceptions from here. By
--        -- catching them, `follow` will try to establish a new connection
--        -- depending on the `FollowExceptionRecovery`.
--        handleSyncOrAsync (traceException *> const (pure FollowFailure))
--      where
--        traceException :: SomeException -> IO ()
--        traceException e = do
--            traceWith tr $ MsgUnhandledException $ T.pack $ show e
--
--    -- | Wait a short delay before querying for blocks again. We also take this
--    -- opportunity to refresh the chain tip as it has probably increased in
--    -- order to refine our syncing status.
--    sleep :: Int -> Bool -> Cursor -> IO FollowExit
--    sleep delay hasRolledForward cursor = do
--            when (delay > 0) (threadDelay delay)
--            step hasRolledForward cursor
--
--    step :: Bool -> Cursor -> IO FollowExit
--    step hasRolledForward cursor = nextBlocks nl cursor >>= \case
--        RollForward cursor' _ [] -> do
--            -- FIXME Make RollForward return NE
--            -- This case seems to never happen.
--            sleep delay0 hasRolledForward cursor'
--
--        RollForward cursor' tip (blockFirst : blocksRest) -> do
--            let blocks = blockFirst :| blocksRest
--            traceWith tr $ MsgApplyBlocks tip (header <$> blocks)
--            action <- yield blocks tip innerTr
--            traceWith tr $ MsgFollowAction (fmap show action)
--            continueWith cursor' True action
--
--        RollBackward cursor' ->
--            -- After negotiating a tip, the node asks us to rollback to the
--            -- intersection. We may have to rollback to our /current/ tip.
--            --
--            -- This would do nothing, but @follow@ handles rollback by exiting
--            -- such that a new negotiation is required, leading to an infinite
--            -- loop.
--            --
--            -- So this becomes a bit intricate:
--
--            case (cursorSlotNo nl cursor', cps, hasRolledForward) of
--                (sl, [], False) -> do
--                    -- The following started from @Origin@.
--                    -- This is the initial rollback.
--                    -- We can infer that we are asked to rollback to Origin, and
--                    -- we can ignore it.
--                    traceWith tr $ MsgWillIgnoreRollback sl "initial rollback, \
--                        \cps=[]"
--                    step hasRolledForward cursor'
--                (sl, _:_, False) | sl == slotNo (last cps) -> do
--                    traceWith tr $ MsgWillIgnoreRollback sl "initial rollback, \
--                        \rollback point equals the last checkpoint"
--                    step hasRolledForward cursor'
--                (sl, _, _) -> do
--                    traceWith tr $ MsgWillRollback sl
--                    destroyCursor nl cursor' $> FollowRollback sl
--            -- Some alternative solutions would be to:
--            -- 1. Make sure we have a @BlockHeader@/@SlotNo@ for @Origin@
--            -- 2. Stop forcing @follow@ to quit on rollback
--    continueWith
--        :: Cursor
--        -> Bool
--        -> FollowAction e
--        -> IO FollowExit
--    continueWith cursor' hrf = \case
--        ExitWith _ -> -- NOTE error logged as part of `MsgFollowAction`
--            return FollowDone
--        Continue ->
--            step hrf cursor'
--

-- | A collection of callbacks to use with the 'chainSync' function.
data ChainFollower m point tip block = ChainFollower
    { readLocalTip :: m [point]
        -- ^ Callback for reading the local tip. Used to negotiate the
        -- intersection with the node.
        --
        -- A response of [] is interpreted as `Origin` -- i.e. the chain will be
        -- served from genesis.
        --
        -- TODO: Could be named readCheckpoints?
    , rollForward :: NonEmpty block -> tip -> m ()
        -- ^ Callback for rolling forward.
        --
        -- Implementors _may_ delete old checkpoints while rolling forward.

    , rollBackward :: point -> m point
        -- ^ Roll back to the requested slot, or further, and return the point
        -- actually rolled back to.
        --
        -- __Example 1:__
        --
        -- If the follower stores checkpoints for all blocks, we can always roll
        -- back to the requested point exactly.
        --
        -- @
        -- -- If
        -- knownSlots follower `shouldReturn` [0,1,2,3]
        -- let requested = SlotNo 2
        -- -- Then
        -- actual <- rollBackward follower requested
        -- knownSlots follower shouldReturn` [0,1,2]
        -- actual `shouldBe` SlotNo 2
        -- @
        --
        -- Note that the slotNos are unlikely to be consecutive in real life,
        -- but this doesn't matter, as ouroboros-network asks us to rollback to
        -- points, corresponding to blocks.
        --
        -- __Example 2:__
        --
        -- @
        -- -- If
        -- knownSlots follower `shouldReturn` [0,9,10]
        -- let requested = SlotNo 2
        -- -- Then
        -- actual <- rollBackward follower requested
        -- knownSlots follower shouldReturn` [0]
        -- actual `shouldBe` SlotNo 0
        -- @
        --
    }

mapChainFollower
    :: Functor m
    => (point1 -> point2) -- ^ Covariant
    -> (point2 -> point1) -- ^ Contravariant
    -> (tip2 -> tip1) -- ^ Contravariant
    -> (block2 -> block1) -- ^ Contravariant
    -> ChainFollower m point1 tip1 block1
    -> ChainFollower m point2 tip2 block2
mapChainFollower fpoint12 fpoint21 ftip fblock cf =
    ChainFollower
        { readLocalTip = map fpoint12 <$> readLocalTip cf
        , rollForward = \bs tip -> rollForward cf (fmap fblock bs) (ftip tip)
        , rollBackward = fmap fpoint12 . rollBackward cf . fpoint21
        }

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}

-- | Low-level logs of the ChainSync mini-protocol
data ChainSyncLog block point
    = MsgChainFindIntersect [point]
    | MsgChainRollForward (NonEmpty block) point
    | MsgChainRollBackward point Int
    | MsgChainTip point
    | MsgLocalTip point
    | MsgTipDistance Natural
    deriving (Show, Eq, Generic)

mapChainSyncLog
    :: (b1 -> b2)
    -> (p1 -> p2)
    -> ChainSyncLog b1 p1
    -> ChainSyncLog b2 p2
mapChainSyncLog f g = \case
    MsgChainFindIntersect points -> MsgChainFindIntersect (g <$> points)
    MsgChainRollForward blocks tip ->
        MsgChainRollForward (f <$> blocks) (g tip)
    MsgChainRollBackward point n -> MsgChainRollBackward (g point) n
    MsgChainTip point -> MsgChainTip (g point)
    MsgLocalTip point -> MsgLocalTip (g point)
    MsgTipDistance d -> MsgTipDistance d

instance ToText (ChainSyncLog BlockHeader ChainPoint) where
    toText = \case
        MsgChainFindIntersect cps -> mconcat
            [ "Requesting intersection using "
            , toText (length cps)
            , " points"
            , maybe "" ((", the latest being " <>) . pretty) (lastMay cps)
            ]
        MsgChainRollForward headers tip ->
            let buildRange (x :| []) = x
                buildRange xs = NE.head xs <> ".." <> NE.last xs
                slots = pretty . slotNo <$> headers
            in mconcat
                [ "ChainSync roll forward: "
                , "applying blocks at slots [", buildRange slots, "]"
                , ", tip is "
                , pretty tip
                ]
        MsgChainRollBackward b 0 ->
            "ChainSync roll backward: " <> pretty b
        MsgChainRollBackward b bufferSize -> mconcat
            [ "ChainSync roll backward: "
            , pretty b
            , ", handled inside pipeline buffer with remaining length "
            , toText bufferSize
            ]
        MsgChainTip tip ->
            "Node tip is " <> pretty tip
        MsgLocalTip point ->
            "Synchronized with point: " <> pretty point
        MsgTipDistance d -> "Distance to chain tip: " <> toText d <> " blocks"

instance HasPrivacyAnnotation (ChainSyncLog block point)

instance HasSeverityAnnotation (ChainSyncLog block point) where
    getSeverityAnnotation = \case
        MsgChainFindIntersect{} -> Debug
        MsgChainRollForward{} -> Debug
        MsgChainRollBackward{} -> Debug
        MsgChainTip{} -> Debug
        MsgLocalTip{} -> Debug
        MsgTipDistance{} -> Debug

-- | Higher level log of a chain follower.
-- Includes computed statistics about synchronization progress.
data ChainFollowLog
    = MsgChainSync (ChainSyncLog BlockHeader ChainPoint)
    | MsgFollowStats (FollowStats History)
    | MsgStartFollowing
    deriving (Show, Eq, Generic)

instance ToText ChainFollowLog where
    toText = \case
        MsgChainSync msg -> toText msg
        MsgFollowStats s -> toText s
        MsgStartFollowing -> "Chain following starting."

instance HasPrivacyAnnotation ChainFollowLog
instance HasSeverityAnnotation ChainFollowLog where
    getSeverityAnnotation = \case
        MsgChainSync msg -> getSeverityAnnotation msg
        MsgFollowStats s -> getSeverityAnnotation s
        MsgStartFollowing -> Info

{-------------------------------------------------------------------------------
    Log aggregation
-------------------------------------------------------------------------------}
-- | Statistics of interest from the follow-function.
--
-- The @f@ allows us to use 'History' to keep track of both current and
-- previously logged stats, and perform operations over it in a nice way.
data FollowStats f = FollowStats
    { blocksApplied :: !(f Int)
    , rollbacks :: !(f Int)
    , localTip :: !(f ChainPoint)
    , time :: !(f UTCTime)
      -- ^ NOTE: Current time is not updated until @flush@ is called.
    , prog :: !(f SyncProgress)
      -- ^ NOTE: prog is not updated until @flush@ is called.
    } deriving (Generic)

-- It seems UTCTime contains thunks internally. This shouldn't matter as we
-- 1. Change it seldom - from @flush@, not from @updateStats@
-- 2. Set to a completely new value when we do change it.
deriving via (AllowThunksIn '["time"] (FollowStats History))
    instance (NoThunks (FollowStats History))

deriving instance Show (FollowStats History)
deriving instance Eq (FollowStats History)

-- | Change the @f@ wrapping each record field.
hoistStats
    :: (forall a. f a -> g a)
    -> FollowStats f
    -> FollowStats g
hoistStats f (FollowStats a b c d e) =
    FollowStats (f a) (f b) (f c) (f d) (f e)

-- | A 'History' consists of a past value and a present value.
-- Useful for keeping track of past logs.
--
-- The idea is to
-- 1. Reconstruct a model of the @current@ @state@ using a @Trace@
-- 2. Sometimes log the difference between the @current@ state and the most
-- recently logged one.
data History a = History
    { past :: !a -- ^ Most previously logged state
    , current :: !a -- ^ Not-yet logged state
    } deriving (Eq, Show, Functor, Generic)

instance NoThunks a => NoThunks (History a)

initHistory :: a -> History a
initHistory a = History a a

-- | Modify the present state of a @History state@
overCurrent :: (a -> a) -> History a -> History a
overCurrent f (History pas cur) = History pas (f cur)

emptyStats :: UTCTime -> FollowStats History
emptyStats t = FollowStats (f 0) (f 0) (f ChainPointAtGenesis) (f t) (f p)
  where
    f = initHistory
    p = NotResponding -- Hijacked as an initial value for simplicity.

-- | Update the current statistics based on a new log message.
updateStats
    :: ChainSyncLog block ChainPoint
    -> FollowStats History -> FollowStats History
updateStats msg s = case msg of
    MsgChainRollForward blocks _tip ->
        s { blocksApplied = overCurrent (+ NE.length blocks) (blocksApplied s) }
    MsgChainRollBackward _ 0 ->
        -- rolled back in a way that could not be handled by the pipeline buffer
        s { rollbacks = overCurrent (1 +) (rollbacks s) }
    MsgLocalTip point ->
        s { localTip = overCurrent (const point) (localTip s) }
    _ -> s

instance ToText (FollowStats History) where
    toText st@(FollowStats b r tip t progress) =
        syncStatus <> " " <> stats <> sevExpl
      where
        syncStatus = case progress of
            History NotResponding Ready ->
                "In sync."
            History Ready Ready ->
                "Still in sync."
            History NotResponding NotResponding ->
                "Still not syncing."
            History (Syncing _p) Ready ->
                "In sync!"
            History Ready (Syncing p) ->
                "Fell out of sync (" <> (pretty p) <> ")"
            History _ (Syncing p) ->
                "Syncing (" <> (pretty p) <> ")"
            History past_ NotResponding ->
                "Not responding. Previously " <> (pretty past_) <> "."
        stats = mconcat
            [ "Applied " <> pretty (using (-) b) <> " blocks, "
            , pretty (using (-) r) <> " rollbacks "
            , "in the last " <> pretty (using diffUTCTime t) <> ". "
            , "Currently tip is" <> pretty (current tip) <> "."
            ]
          where
            using f x = f (current x) (past x)

        sevExpl = maybe
            ""
            (\x -> " (" <> x <> ")")
            (snd $ explainedSeverityAnnotation st)

-- NOTE: Here we check if the sync progress is going backwards, which
-- would be a sign the wallet is overloaded (or rollbacks)
--
-- But this check might be in the wrong place. Might be better to
-- produce new logs from inside the updateStats function and immeditely
-- warn there.
explainedSeverityAnnotation :: FollowStats History -> (Severity, Maybe Text)
explainedSeverityAnnotation s
    | progressMovedBackwards = (Warning, Just "progress decreased")
    | noBlocks && notRestored = (Warning, Just "not applying blocks")
    | nowInSync = (Notice, Nothing)
    | otherwise = (Info, Nothing)
  where
    progressMovedBackwards = current (prog s) < past (prog s)
    nowInSync = current (prog s) == Ready && past (prog s) < Ready
    notRestored = current (prog s) /= Ready
    noBlocks = (current (blocksApplied s) - past (blocksApplied s)) <= 0

instance HasSeverityAnnotation (FollowStats History) where
    getSeverityAnnotation = fst . explainedSeverityAnnotation

-- | Update the 'TMVar' holding the 'FollowStats'@ @'History'
-- to forget the 'past' values and replace them with the 'current' ones.
-- Also update the time and sync process.
flushStats
    :: UTCTime
    -> (SlotNo -> IO SyncProgress)
    -> StrictTMVar IO (FollowStats History)
    -> IO (FollowStats History)
flushStats t calcSyncProgress var = do
    s <- atomically $ takeTMVar var
    p <- calcSyncProgress $ pseudoPointSlot $ current $ localTip s
    let s' = s { time = overCurrent (const t) (time s) }
               { prog = overCurrent (const p) (prog s) }
    atomically $ putTMVar var $ hoistStats forgetPast s'
    return s'
  where
    forgetPast (History _past curr) = initHistory curr

    -- See NOTE [PointSlotNo].
    -- Fortunately, this is not important for the use here.
    pseudoPointSlot :: ChainPoint -> SlotNo
    pseudoPointSlot ChainPointAtGenesis = SlotNo 0
    pseudoPointSlot (ChainPoint slot _) = slot

-- | Monitors health and statistics by inspecting the messages
-- submitted to a 'ChainSyncLog' tracer.
--
-- Statistics are computed in regular time intervals.
-- In order to do that, the monitor runs in separate thread.
-- The results are submitted to the outer 'ChainFollowLog' tracer.
withFollowStatsMonitoring
    :: Tracer IO ChainFollowLog
    -> (SlotNo -> IO SyncProgress)
    -> (Tracer IO (ChainSyncLog BlockHeader ChainPoint) -> IO ())
    -> IO ()
withFollowStatsMonitoring tr calcSyncProgress act = do
    t0  <- getCurrentTime
    var <- newTMVarIO $ emptyStats t0
    let trChainSyncLog = flip contramapM tr $ \msg -> do
            atomically $ do
                s <- takeTMVar var
                putTMVar var $! updateStats msg s
            pure $ MsgChainSync msg
    traceWith trChainSyncLog $ MsgLocalTip ChainPointAtGenesis
    race_
        (act trChainSyncLog)
        (loop var startupDelay)
  where
    loop var delay = do
        threadDelay delay
        t <- getCurrentTime
        s <- flushStats t calcSyncProgress var
        traceWith tr $ MsgFollowStats s
        let delay' =
                if (current (prog s)) == Ready
                then restoredDelay
                else syncingDelay
        loop var delay'

    -- | Delay from launch to the first status update
    startupDelay = 5 * second
    -- | Delay between status updates when restored
    restoredDelay = 5 * minute
    -- | Delay between status updates when not restored
    syncingDelay = 30 * second

    second = 1000*1000
    minute = 60 * second

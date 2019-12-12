{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.MetadataSpec (spec) where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Setup
    ( setupTrace_ )
import Cardano.BM.Trace
    ( Trace )
import Cardano.Pool.Metadata
    ( FetchError (..)
    , RegistryLog (..)
    , RegistryLogMsg (..)
    , StakePoolMetadata (..)
    , StakePoolTicker
    , getStakePoolMetadata
    )
import Cardano.Wallet.Logging
    ( transformTextTrace )
import Cardano.Wallet.Primitive.Types
    ( PoolOwner (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromText )
import Codec.Archive.Zip
    ( CompressionMethod (..), addEntry, createArchive, mkEntrySelector )
import Control.Monad
    ( forM_, replicateM )
import Data.Aeson
    ( encode )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import System.FilePath
    ( takeDirectory, (<.>), (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, around, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryPrintableChar
    , choose
    , counterexample
    , elements
    , frequency
    , listOf
    , listOf1
    , property
    , shuffle
    , sublistOf
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.StaticServer
    ( withStaticServer )
import Test.Utils.Trace
    ( captureLogging )

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Cardano.Pool.Registry.getStakePoolMetadata specs" $
        around (withStaticServer dataDir) $ do
        it "Loads the example zip" $ \baseUrl -> do
            tr <- setupLogging
            res <- getStakePoolMetadata tr (testUrl baseUrl) presentOwners
            res `shouldBe` Right (map Just presentMetas)

        it "Handles a missing pool" $ \baseUrl -> do
            tr <- setupLogging
            res <- getStakePoolMetadata tr (testUrl baseUrl) (absentOwner:presentOwners)
            res `shouldBe` Right (Nothing:map Just presentMetas)

        it "Fails with an unavailable HTTP server" $ \_port -> do
            tr <- setupLogging
            let badUrl = "http://localhost:99/master.zip"
            res <- getStakePoolMetadata tr badUrl presentOwners
            case res of
                Left (FetchErrorDownload msg) ->
                    head (words msg) `shouldBe` "HttpExceptionRequest"
                _ -> error "expected fetch error but got none"

    describe "Cardano.Pool.Registry.getStakePoolMetadata" $
        it "Arbitrary zip files" $
            property prop_getStakePoolMetadata

{-------------------------------------------------------------------------------
                                 Test fixtures
-------------------------------------------------------------------------------}

dataDir :: FilePath
dataDir = $(getTestData) </> "stake-pool-registry"

-- | Make a file server URL for the test data file.
-- This file was downloaded from <https://github.com/input-output-hk/testnet-stake-pool-registry/archive/master.zip>
testUrl :: String -> String
testUrl baseUrl = baseUrl ++ "testnet-stake-pool-registry-master.zip"

presentOwners :: [PoolOwner]
presentOwners = map unsafeFromText
    [ "ed25519_pk1a6mv6x2r0f3y62ddavxvf5lkdsj8ttc8mk3yngdy895j8tn6jqyqesvuk2"
    , "ed25519_pk1fh2ajuu7rqquuxqrkh6ldfvdktlthkl4wlfhymrath85wzhy7slsr37f87"
    , "ed25519_pk1wc84pj9h3qw5kknawj2d0mqwr43gxhtsnq5pmuzxg7ssrcpt84ds9sce69"
    ]

presentMetas :: [StakePoolMetadata]
presentMetas =
    [ StakePoolMetadata
        { ticker = unsafeFromText "FST"
        , homepage = "https://12345"
        , owner = unsafeFromText "ed25519_pk1a6mv6x2r0f3y62ddavxvf5lkdsj8ttc8mk3yngdy895j8tn6jqyqesvuk2"
        , name = "Ada Lovelace"
        , description = Nothing
        , pledgeAddress = "addr1svklmf8yl78x9cw30ystvprhxtm790k4380xlsjrjqn2p8nekup8uvzfezl"
        }
    , StakePoolMetadata
        { ticker = unsafeFromText "IOHK1"
        , homepage = "https://iohk.io"
        , owner = unsafeFromText "ed25519_pk1fh2ajuu7rqquuxqrkh6ldfvdktlthkl4wlfhymrath85wzhy7slsr37f87"
        , name = "IOHK Pool - 1"
        , description = Nothing
        , pledgeAddress = "addr1s4xatktnncvqrnscqw6lta493ke0aw7m74maxunv04wu73c2un6r7vm0k7z"
        }
    , StakePoolMetadata
        { ticker = unsafeFromText "TICK"
        , homepage = "https://12345"
        , owner = unsafeFromText "ed25519_pk1wc84pj9h3qw5kknawj2d0mqwr43gxhtsnq5pmuzxg7ssrcpt84ds9sce69"
        , name = "Ada Lovelace"
        , description = Nothing
        , pledgeAddress = "addr1svklmf8yl78x9cw30ystvprhxtm790k4380xlsjrjqn2p8nekup8uvzfezl"
        }
    ]

absentOwner :: PoolOwner
absentOwner = PoolOwner "pk192m4ytl5k357e2l666yleuwjlurmf0vxjyh4atxzu5m22q6mexlsp88k7x"

{-------------------------------------------------------------------------------
                                    Property
-------------------------------------------------------------------------------}

data TestCase = TestCase
    { stakePools :: [(PoolOwner, StakePoolEntry)]
    -- ^ Stake pool metadata in the zip file.
    , poolOwners :: [PoolOwner]
    -- ^ Stake pools to look up for the test.
    , topDir :: FilePath
    -- ^ The name of the top-level directory in the zip file.
    } deriving (Show, Eq)

data StakePoolEntry
    = Junk
    | Meta StakePoolMetadata
    deriving (Show, Eq)

-- | Tests looking up stake pools ids in a randomish zip file. Some metadata is
-- missing, some fails to parse. The property asserts that all metadata which
-- exists is looked up correctly, and that any parse errors are logged.
prop_getStakePoolMetadata :: TestCase -> Property
prop_getStakePoolMetadata tc = monadicIO $ do
    -- Generate a zip file for the test case and serve on a local file server.
    -- Run getStakePoolMeta and take the actual result, and the log messages.
    (msgs, res) <- run $ withTestCaseZip tc $ \zipFile ->
        withStaticServer (takeDirectory zipFile) $ \baseUrl -> do
            let url = testCaseUrl tc baseUrl
            captureLogging $ \tr -> getStakePoolMetadata tr url (poolOwners tc)
    let numDecodeErrors = count isDecodeErrorMsg msgs

    -- Expected results
    let expected =
            [ stakePoolEntryMeta =<< lookup p (stakePools tc) | p <- poolOwners tc ]
    let numJunkEntries =
            count (\(p, e) -> isJunk e && p `elem` poolOwners tc) (stakePools tc)

    monitor $ counterexample $ unlines $
        [ "expected        = " ++ show expected
        , "actual          = " ++ show res
        , "# junk entries  = " ++ show numJunkEntries
        , "# decode errors = " ++ show numDecodeErrors
        , "logs ="
        ] ++ map (T.unpack . toText) msgs

    assert $ res == Right expected && numJunkEntries == numDecodeErrors

withTestCaseZip :: TestCase -> (FilePath -> IO a) -> IO a
withTestCaseZip tc action = withSystemTempDirectory "registry-spec" $ \dir -> do
    let zipFile = dir </> topDir tc <.> "zip"
    createArchive zipFile $ forM_ (stakePools tc) $ \(owner, contents) -> do
        sel <- mkEntrySelector (registryFile (topDir tc) owner)
        addEntry Deflate (encodeStakePoolEntry contents) sel
    action zipFile

registryFile :: FilePath -> PoolOwner -> FilePath
registryFile top owner =
    top </> "registry" </> T.unpack (toText owner) <.> "json"

testCaseUrl :: TestCase -> String -> String
testCaseUrl tc baseUrl = baseUrl ++ topDir tc ++ ".zip"

{-------------------------------------------------------------------------------
                               Test case helpers
-------------------------------------------------------------------------------}

isDecodeErrorMsg :: RegistryLog -> Bool
isDecodeErrorMsg rl = case registryLogMsg rl of
    MsgExtractFileResult (Just (Left _)) -> True
    _ -> False

stakePoolEntryMeta :: StakePoolEntry -> Maybe StakePoolMetadata
stakePoolEntryMeta Junk = Nothing
stakePoolEntryMeta (Meta m) = Just m

encodeStakePoolEntry :: StakePoolEntry -> ByteString
encodeStakePoolEntry Junk = "junk"
encodeStakePoolEntry (Meta m) = BL.toStrict $ encode m

isJunk :: StakePoolEntry -> Bool
isJunk Junk = True
isJunk _ = False

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

{-------------------------------------------------------------------------------
                              Test case generation
-------------------------------------------------------------------------------}

instance Arbitrary TestCase where
    arbitrary = do
        stakePools <- arbitrary
        presentPoolIds <- sublistOf (map fst stakePools)
        absentPoolIds <- arbitrary
        poolOwners <- shuffle (presentPoolIds ++ absentPoolIds)
        PathElement topDir <- arbitrary
        pure $ TestCase {stakePools, poolOwners, topDir}
    shrink (TestCase sps pids td)
        = [ TestCase sps' pids' td'
          | (sps', pids', td') <- shrink (sps, pids, td)
          , not (null td')
          ]

instance Arbitrary StakePoolEntry where
    arbitrary = frequency
        [ (1, pure Junk)
        , (4, Meta <$> arbitrary)
        ]

instance Arbitrary StakePoolMetadata where
    arbitrary = StakePoolMetadata
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Text where
    arbitrary = T.pack <$> listOf c
      where
        c = frequency
            [ (6, arbitraryPrintableChar)
            , (1, arbitrary)
            , (1, elements ['☃', 'ż', '好'])
            ]

instance Arbitrary StakePoolTicker where
    arbitrary = unsafeFromText . T.pack <$> do
        len <- choose (3, 5)
        replicateM len arbitrary

instance Arbitrary PoolOwner where
    arbitrary = PoolOwner . B8.pack <$> vectorOf 32 c
        where c = elements ['a'..'z']

newtype PathElement = PathElement FilePath deriving (Show, Eq)

instance Arbitrary PathElement where
    arbitrary = PathElement <$> listOf1 (elements ['a'..'z'])

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

setupLogging :: IO (Trace IO RegistryLog)
setupLogging = do
    cfg <- defaultConfigStdout
    transformTextTrace . fst <$> setupTrace_ cfg "tests"

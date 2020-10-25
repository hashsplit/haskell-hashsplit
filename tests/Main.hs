{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
module Main where

import Zhp

import Test.Hspec

import Data.Aeson
import GHC.Generics     (Generic)
import System.Directory (listDirectory)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Map.Strict       as M
import qualified HashSplit             as HS
import           HashSplit.Classes
import           HashSplit.Hashes.CP32 (CP32)
import           HashSplit.Hashes.RRS1 (RRS1)

data Configs = Configs
    { cases :: M.Map String Case
    }
    deriving(Generic)
instance ToJSON Configs
instance FromJSON Configs

data Case = Case
    { config :: Config
    , sizes  :: [Int]
    }
    deriving(Generic)
instance ToJSON Case
instance FromJSON Case

data Config = Config
    { minSize   :: !Int
    , maxSize   :: !Int
    , threshold :: !Int
    , hash      :: String
    }
    deriving(Generic)
instance ToJSON Config
instance FromJSON Config

loadConfigs :: FilePath -> IO Configs
loadConfigs path = do
    txt <- LBS.readFile (path <> "/configs.json")
    case decode txt of
        Nothing  -> error "Failed to decode configs"
        Just cfg -> pure cfg

loadInput :: FilePath -> IO LBS.ByteString
loadInput path =
    LBS.readFile (path <> "/input")

split :: AnyConfig -> LBS.ByteString -> [BS.ByteString]
split (AnyConfig cfg) input = HS.split cfg input

runCase :: String -> Case -> LBS.ByteString -> Spec
runCase name Case{config, sizes} input = describe name $ do
    let cfg = parseConfig config
        chunks = split cfg input
        actualSizes = map BS.length chunks
    it "Should produce the right sizes" $
        actualSizes `shouldBe` sizes
    it "Should produce chunks that match the input" $
        LBS.concat (map LBS.fromStrict chunks) `shouldBe` input

runDir :: FilePath -> Spec
runDir path = describe ("Tests under " <> path) $ do
    configs <- runIO $ loadConfigs path
    input <- runIO $ loadInput path
    let caseList = M.toList (cases configs)
    for_ caseList $ \(name, c) ->
        runCase name c input

runSubdirsOf :: FilePath -> Spec
runSubdirsOf path = do
    describe ("Tests under " <> path) $ do
        dirs <- runIO $ listDirectory path
        for_ dirs $ \d ->
            runDir (path <> "/" <> d)

data AnyConfig where
    AnyConfig :: (RollingHash h, Bits (Digest h), Num (Digest h)) => HS.Config h -> AnyConfig

parseConfig :: Config -> AnyConfig
parseConfig config =
    let mkConfig :: (RollingHash h, Bits (Digest h), Num (Digest h)) => Proxy h -> AnyConfig
        mkConfig proxy =
            AnyConfig HS.Config
                { HS.cfgMinSize = minSize config
                , HS.cfgMaxSize = maxSize config
                , HS.cfgHash = proxy
                , HS.cfgThreshold = threshold config
                }
    in
    case hash config of
        "rrs1" -> mkConfig (Proxy :: Proxy RRS1)
        "cp32" -> mkConfig (Proxy :: Proxy CP32)
        fnName -> error $ "Unsupported hash function: " <> fnName

main :: IO ()
main = hspec $ runSubdirsOf "testdata/tests"

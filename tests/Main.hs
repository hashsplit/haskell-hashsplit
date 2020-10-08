{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Zhp

import Data.Aeson
import GHC.Generics     (Generic)
import System.Directory (listDirectory)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Map.Strict       as M
import qualified HashSplit             as HS
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

runCase :: String -> Case -> LBS.ByteString -> IO ()
runCase name Case{config, sizes} input = do
    putStrLn $ "Running test case: " <> name
    cfg <- parseConfig config
    let chunks = HS.split cfg input
        actualSizes = map BS.length chunks
    when (sizes /= actualSizes) $ do
        putStrLn $
            "Wrong chunks; expected:\n\n\t" <>
            show sizes <> "\n\n" <>
            "But got:\n\n\t" <>
            show actualSizes
        error "wrong chunks"
    when (LBS.concat (map LBS.fromStrict chunks) /= input) $ do
        error "Failed 'concat chunks == input'."

runDir :: FilePath -> IO ()
runDir path = do
    putStrLn $ "Running tests in " <> path
    configs <- loadConfigs path
    input <- loadInput path
    let caseList = M.toList (cases configs)
    for_ caseList $ \(name, c) ->
        runCase name c input

runSubdirsOf :: FilePath -> IO ()
runSubdirsOf path = do
    putStrLn $ "Running all tests under " <> path
    dirs <- listDirectory path
    for_ dirs $ \d ->
        runDir (path <> "/" <> d)

parseConfig :: Config -> IO (HS.Config RRS1)
parseConfig config = do
    hashFn <- case hash config of
        "rrs1" -> pure (Proxy :: Proxy RRS1)
        fnName -> error $ "Unsupported hash function: " <> fnName
    pure HS.Config
        { HS.cfgMinSize = minSize config
        , HS.cfgMaxSize = maxSize config
        , HS.cfgHash = hashFn
        , HS.cfgThreshold = threshold config
        }

main :: IO ()
main = runSubdirsOf "testdata/tests"

{-# LANGUAGE DoAndIfThenElse  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE StrictData       #-}
module HashSplit
    ( Config(..)
    , SplitState
    , newState
    , addByte
    ) where

import Zhp

import           Control.Monad.ST
import qualified Data.ByteString    as BS
import           Data.Default.Class (def)
import           Data.Proxy         (Proxy)
import           Data.STRef

import           HashSplit.Classes
import           HashSplit.Internal.Chunk      (Chunk)
import qualified HashSplit.Internal.Chunk      as Chunk
import           HashSplit.Internal.RingBuffer (RingBuffer)
import qualified HashSplit.Internal.RingBuffer as RingBuffer

data Config h = Config
    { cfgMinSize   :: !Int
    , cfgMaxSize   :: !Int
    , cfgHash      :: Proxy h
    , cfgThreshold :: !Int
    }

data SplitState s h = SplitState
    { ringBuf   :: RingBuffer s
    , hashState :: STRef s h
    , chunk     :: Chunk s
    , config    :: Config h
    }

newState :: RollingHash h => Config h -> ST s (SplitState s h)
newState config = do
    ringBuf <- RingBuffer.new (windowSize (cfgHash config))
    chunk <- Chunk.new (cfgMinSize config)
    hashState <- newSTRef def
    pure SplitState
        { ringBuf
        , hashState
        , chunk
        , config
        }

addByte :: (RollingHash h, Bits (Digest h), Num (Digest h)) => Word8 -> SplitState s h -> ST s (Maybe BS.ByteString)
addByte newByte SplitState{ringBuf, hashState, chunk, config} = do
    Chunk.addByte newByte chunk
    oldByte <- RingBuffer.push newByte ringBuf
    state <- readSTRef hashState
    let state' = roll newByte oldByte state
    writeSTRef hashState $! state'
    chunkLen <- Chunk.length chunk
    if chunkLen < cfgMinSize config then
        don'tSplit
    else if chunkLen == cfgMaxSize config then
        split
    else if digest state `matches` cfgThreshold config then
        split
    else
        don'tSplit
  where
    split = Just <$> Chunk.takeBytes chunk
    don'tSplit = pure Nothing
    matches d t = ((1 `shiftL` t) - 1) .&. d == 0

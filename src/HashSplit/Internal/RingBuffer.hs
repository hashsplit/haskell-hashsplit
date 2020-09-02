{-# LANGUAGE NamedFieldPuns #-}
-- | Implementation of a simple ring buffer/circular queue.
module HashSplit.Internal.RingBuffer
    ( RingBuffer
    , new
    , push
    ) where

import Zhp

import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Generic.Mutable as GV
import qualified Data.Vector.Unboxed.Mutable as UV

-- | A 'RingBuffer' is a mutable queue with a fixed number of elements.
-- the element type is fixed as 'Word8'.
data RingBuffer s = RingBuffer
    { buffer :: !(UV.MVector s Word8)
    , offset :: !(STRef s Int)
    }

-- | @'new' size@ creates a new @RingBuffer@ of the given size, filling it
-- with zeros.
new :: Int -> ST s (RingBuffer s)
new len = do
    offset <- newSTRef 0
    buffer <- UV.replicate len 0
    pure RingBuffer { buffer, offset }

-- | @'push' byte buffer@ adds @byte@ to the ringbuffer, and dequeues & returns
-- the next element from the queue.
push :: Word8 -> RingBuffer s -> ST s Word8
push add RingBuffer{buffer, offset} = do
    index <- readSTRef offset
    del <- GV.exchange buffer index add
    writeSTRef offset $! (index + 1) `mod` UV.length buffer
    pure del

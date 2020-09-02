{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData     #-}
module HashSplit.Internal.Chunk
    ( Chunk
    , new
    , length
    , addByte
    , reset
    , readBytes
    , takeBytes
    ) where

import Zhp hiding (length)

import           Control.Monad.ST
import qualified Data.ByteString              as BS
import           Data.ByteString.Internal     (ByteString(PS))
import           Data.STRef
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as SMV

data Chunk s = Chunk
    { lenRef :: STRef s Int
    , vecRef :: STRef s (SV.MVector s Word8)
    }

-- | @'new' capacity@ creates a new empty chunk with underlying storage
-- of the given size.
new :: Int -> ST s (Chunk s)
new capacity = do
    vec <- SMV.new capacity
    vecRef <- newSTRef vec
    lenRef <- newSTRef 0
    pure Chunk{vecRef, lenRef}

length :: Chunk s -> ST s Int
length = readSTRef . lenRef

-- | Add a byte to the end of the chunk.
addByte :: Word8 -> Chunk s -> ST s ()
addByte byte chunk = do
    ensureSpace chunk
    len <- readSTRef $ lenRef chunk
    vec <- readSTRef $ vecRef chunk
    SMV.write vec len byte

-- | Ensure that there is space for at least one more byte in the chunk's buffer.
ensureSpace :: Chunk s -> ST s ()
ensureSpace Chunk{lenRef, vecRef} = do
    len <- readSTRef lenRef
    vec <- readSTRef vecRef
    when (len == SMV.length vec) $ do
        vec' <- SMV.new (len * 2 + 1)
        for [0..len - 1] $ \i -> do
            v <- SMV.read vec i
            SMV.write vec' i v
        writeSTRef vecRef vec'

-- | Reset the chunk to empty. Does not free or shrink the underlying buffer.
reset :: Chunk s -> ST s ()
reset Chunk{lenRef} =
    writeSTRef lenRef 0

-- | Read the underlying bytes from the chunk.
readBytes :: Chunk s -> ST s BS.ByteString
readBytes Chunk{lenRef, vecRef} = do
    len <- readSTRef lenRef
    vec <- SMV.take (len - 1) <$> readSTRef vecRef
    vec' <- SV.freeze vec
    let (fptr, offset, len) = SV.unsafeToForeignPtr vec'
    pure $ PS fptr offset len

-- | 'readBytes' followed by 'reset'.
takeBytes :: Chunk s -> ST s BS.ByteString
takeBytes chunk = do
    bytes <- readBytes chunk
    reset chunk
    pure bytes

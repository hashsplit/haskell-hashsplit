{-# LANGUAGE TypeFamilies #-}
module HashSplit.Classes
    ( RollingHash(..)
    ) where

import Zhp

import Data.Default.Class (Default)

-- | An instance of 'RollingHash' is a rolling hash function:
--
-- https://en.wikipedia.org/wiki/Rolling_hash
--
-- The type @h@ represents the state used to compute the hash incrementally.
--
-- The 'Default' instance's 'def' should represent the state for an all-zero
-- window.
class Default h => RollingHash h where
    -- The output type of the hash.
    type Digest h

    -- | The size of the window used by the rolling hash.
    windowSize :: proxy h -> Int

    -- | @'roll' add remove state@ slides the window of @state@ by 1 byte,
    -- removing @remove@ and adding @add@.
    roll :: Word8 -> Word8 -> h -> h

    -- | Compute the digest for the current window.
    digest :: h -> Digest h

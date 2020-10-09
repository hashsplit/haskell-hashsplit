{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}
-- | The RRS1 rolling hash function.
module HashSplit.Hashes.RRS1
    ( RRS1
    ) where

import Zhp

import Data.Default.Class
import HashSplit.Classes

-- | State for the RRS1 rolling hash.
data RRS1 = RRS1
    { a :: !Word32
    , b :: !Word32
    }

charOffset = 31

wSize :: Word32
wSize = 64

instance Default RRS1 where
    def = RRS1
        { a = charOffset * wSize
        , b = charOffset * wSize * (wSize - 1)
        }

instance RollingHash RRS1 where
    type Digest RRS1 = Word32

    windowSize _ = fromIntegral wSize

    roll add remove RRS1{a, b} =
        let a' = a - (fromIntegral remove + charOffset) + (fromIntegral add + charOffset) in
        RRS1
            { a = a'
            , b = b - wSize * (fromIntegral remove + charOffset) + a'
            }

    digest RRS1{a, b} = b .|. (a `shiftL` 16)

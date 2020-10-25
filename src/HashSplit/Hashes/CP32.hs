{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module HashSplit.Hashes.CP32
    ( CP32
    ) where

import           Data.Default.Class
import qualified Data.Vector.Unboxed as UV
import           HashSplit.Classes
import           Zhp

newtype CP32 = CP32 Word32
    deriving(Default)

instance RollingHash CP32 where
    type Digest CP32 = Word32

    windowSize _ = 64
    roll add remove (CP32 d) =
        CP32 $ (d `rotateL` 1) `xor` g add `xor` g remove
    digest (CP32 d) = d

g :: Word8 -> Word32
g i = gTable UV.! fromIntegral i

gTable :: UV.Vector Word32
gTable = UV.fromList [
    0x6b326ac4, 0x13f8e1bd, 0x1d61066f, 0x87733fc7, 0x37145391, 0x1c115e40,
    0xd2ea17a3, 0x8650e4b1, 0xe892bb09, 0x408a0c3a, 0x3c40b72c, 0x2a988fb0,
    0xf691d0f8, 0xb22072d9, 0x6fa8b705, 0x72bd6386, 0xdd905ac3, 0x7fcba0ba,
    0x4f84a51c, 0x1dd8477e, 0x6f972f2c, 0xaccd018e, 0xe2964f13, 0x7a7d2388,
    0xebf42ca7, 0xa8e2a0a2, 0x8eb726d3, 0xccd169b6, 0x5444f61e, 0xe178ad7a,
    0xd556a18d, 0xbac80ef4, 0x34cb8a87, 0x7740a1a9, 0x62640fe1, 0xb1e64472,
    0xdee2d6c8, 0x27849114, 0xb6333f4b, 0xbb0b5c1d, 0x57e53652, 0xfde51999,
    0xef773313, 0x1bbaf941, 0x2e9aa084, 0x37587ab8, 0xa61e7c54, 0xb779be61,
    0xd8795bfd, 0x1707c1f6, 0x50fe9c54, 0x32ff3685, 0x94f55c22, 0x2a32ce1a,
    0x0b9076ab, 0x14363079, 0xae994b2c, 0x4a8da881, 0x4770b9c4, 0xf4d143dd,
    0x70a90c0b, 0xa094582a, 0x4b254d10, 0x2454325e, 0x1725a589, 0x9a3380da,
    0x948eeade, 0x79f88224, 0x7b8dc378, 0xc2090db6, 0x41f7a7ac, 0xd4d9528c,
    0x7f0bace7, 0xd3157814, 0xd7757bc4, 0xb428db06, 0x2e2b1d02, 0x0499bcf5,
    0x310f963e, 0xe5f31a83, 0xe0cd600f, 0x8b48af14, 0x568eb23a, 0x01d1150b,
    0x33f54023, 0xa0e59fdf, 0x8d17c2dd, 0xfb7bd347, 0x4d8cd432, 0x664db8de,
    0xd48f2a6c, 0x16c3412d, 0x873a32fc, 0x10796a21, 0xed40f0f8, 0x5ca8e9b2,
    0x0f70d259, 0x0df532c2, 0x016d73aa, 0x45761aa5, 0x189b45a7, 0x4accd733,
    0x641f90e3, 0x592ed9ee, 0x4b1d72ad, 0x42ff2cd4, 0x0654b609, 0x799012c0,
    0x595f36a4, 0x082bdbd6, 0x0375ddd3, 0xc16c1fb5, 0x57492df8, 0xa2d56a98,
    0xdfb2aa28, 0x3728f35f, 0xdc49ea71, 0x9aee8377, 0xd62de2ab, 0x2c3aa155,
    0x407d9eed, 0xbc5b3832, 0x42961924, 0x1498172a, 0xc7126716, 0x95494b56,
    0xd40442fb, 0xb22a3ed1, 0x0ad3e0ae, 0x77a6136a, 0xfb1bc3f0, 0x1a715c38,
    0xccbbd21d, 0x061ff037, 0x85d700cb, 0x8a8fb396, 0x956bbe48, 0xf2556ed8,
    0x3319c88b, 0xe0d6d3e9, 0x4783b316, 0x03a73543, 0x253be5ed, 0x41322aea,
    0xdfc00c7a, 0x972b9413, 0xccca42f5, 0x0a1cdf35, 0xa2dc31b8, 0xf48397eb,
    0xbe3f2b3e, 0xd2950b9f, 0xccd269cf, 0x51a64ca9, 0xea46d96e, 0xcaec892e,
    0x3fae3a62, 0xf12e53db, 0x3753464c, 0x214fbd91, 0x609ce2f7, 0x6158b44c,
    0xa74b8027, 0x79f36912, 0x16cac162, 0x5e76df4f, 0xbc4184fb, 0x912cac7d,
    0xf97e5704, 0x664dd25f, 0x7d837805, 0x5386cfe0, 0x4e585d77, 0xa0fa527e,
    0xeb5c8401, 0xa186cc51, 0x05ef3f1f, 0xc1efc774, 0x38730c2c, 0xad9c5539,
    0x27cd4938, 0x7317b4f2, 0x852c186f, 0xa4c9b0f4, 0xf592f010, 0xf6fe86f3,
    0xb14ba86c, 0x07109a27, 0x0d00568d, 0xd92ee49f, 0xdc643eb3, 0x8d81c333,
    0xcd1d7bbd, 0x87ff9cda, 0x80fa4285, 0x25258d5b, 0xd9e4065a, 0x78955c18,
    0x84874c2a, 0xfdae136b, 0x48eeb3d3, 0xc2623958, 0x5a74f96d, 0x0bcb49f5,
    0x3041cefc, 0xa5b0a1a8, 0x2d29bae6, 0x916ace93, 0x0e70564d, 0xa24894ae,
    0x9897044d, 0xcba97c2a, 0x52a313b1, 0x318ec481, 0xc4729ec1, 0xd90ad78a,
    0x55eb9f90, 0x4f159fda, 0xa90fbd44, 0xd0ca6208, 0x5c597269, 0xe05a471e,
    0x26a5e224, 0x97144944, 0xece2c486, 0xf65c9a9e, 0x82a3fbbb, 0x925d1a62,
    0xd6c4c29b, 0x61b9292d, 0x161529c9, 0x37713240, 0x68ec933b, 0xed80a4e5,
    0x02b2db41, 0x47cfd676, 0xbfe26b41, 0x5e8468bb, 0x6e0d15a4, 0x40383ef4,
    0x81e622fb, 0x194b378c, 0x0c503af5, 0x8e0033a7, 0x003aaa5e, 0x9d7b6723,
    0x0702e877, 0x34b75166, 0xd1ba98d8, 0x9b9f1794, 0xe8961c84, 0x9d773b17,
    0xf9783ee9, 0xdff11758, 0x49bea2cf, 0xa0e0887f
    ]
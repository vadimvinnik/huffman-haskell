module Huffman.Binary (
  byteToBits,
  packBits,
  decompress
) where

import Huffman
import qualified Data.Word
import qualified Data.Binary.BitBuilder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Bits
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import GHC.Int

type Byte = Data.Word.Word8

byteToBits :: Byte -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

packBits :: [Bool] -> B.BitBuilder
packBits = foldl B.append B.empty . map B.singleton

decompress :: L.ByteString -> L.ByteString
decompress bs0 = L.pack $ take (fromIntegral n) $ decode t bs6
  where
    (n, bs1, _)  = runGetState getWord64be bs0 0
    (m, bs2, _)  = runGetState getWord8 bs1 0
    (bs3, bs4)   = L.splitAt ((fromIntegral m) + 1) bs2
    bs5          = concat $ map byteToBits $ L.unpack bs4
    (t, bs6)     = restoreTree (L.unpack bs3) bs5


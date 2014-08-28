module Huffman.Binary (
  compressWithLength,
  compress,
  decompress
) where

import Huffman
import qualified Data.Word
import qualified Data.Binary.BitBuilder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as U
import qualified Data.Map as M
import Data.Bits
import Data.Char
import Data.Binary.Get
import GHC.Int

type Byte = Data.Word.Word8

byteToBits :: Byte -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

bytesToHuffmanTree :: L.ByteString -> HuffmanTree Byte
bytesToHuffmanTree = toTree [] . L.unpack

encodeBinary :: M.Map Byte [Bool] -> L.ByteString -> B.BitBuilder
encodeBinary m = packBits . encode m . L.unpack

packBits :: [Bool] -> B.BitBuilder
packBits = foldl B.append B.empty . map B.singleton

serializeTreeBinary :: HuffmanTree Byte -> B.BitBuilder
serializeTreeBinary t = B.append (B.fromBits 8 $ length as) (B.append (B.fromLazyByteString $ L.pack as) (packBits ns)) where
  (as, ns) = serializeTree t

compressWithLength :: Int64 -> L.ByteString -> L.ByteString
compressWithLength n bs = L.append a (B.toLazyByteString $ B.append b c)  where
  a = U.toLazyByteString $ U.int64BE n
  b = serializeTreeBinary t
  c = encodeBinary m bs
  t = bytesToHuffmanTree bs
  m = toCodeTable t

compress :: L.ByteString -> L.ByteString
compress bs = compressWithLength (L.length bs) bs

decompress :: L.ByteString -> (L.ByteString, HuffmanTree Byte)
decompress bs0 = (L.take (toEnum $ fromEnum n) $ L.pack $ decode t bs6, t) where
  (n, bs1, _)  = runGetState getWord64be bs0 0
  (m, bs2, _)  = runGetState getWord8 bs1 0
  (bs3, bs4)   = L.splitAt (toEnum $ fromEnum m) bs2
  bs5          = concat $ map byteToBits $ L.unpack bs4
  (t, bs6)     = deserializeTree (L.unpack bs3) bs5

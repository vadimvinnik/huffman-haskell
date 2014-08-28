module Huffman.Binary (
  bytesToHuffmanTree,
  encodeBinary,
  decodeBinary, 
  serializeTreeBinary,
  deserializeTreeBinary,
  compressWithLength,
  compress
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

encodeBinary :: M.Map Byte [Bool] -> L.ByteString -> L.ByteString
encodeBinary m = packBits . encode m . L.unpack

packBits :: [Bool] -> L.ByteString
packBits =
  B.toLazyByteString .
  foldl B.append B.empty .
  map B.singleton

decodeBinary :: HuffmanTree Byte -> Int64 -> L.ByteString -> L.ByteString
decodeBinary t n = L.take n . L.pack . (decode t) . concat . (map byteToBits) . L.unpack

serializeTreeBinary :: HuffmanTree Byte -> L.ByteString
serializeTreeBinary t = L.append (L.pack ((toEnum $ length as :: Byte):as)) (packBits ns) where
  (as, ns) = serializeTree t

deserializeTreeBinary :: L.ByteString -> HuffmanTree Byte
deserializeTreeBinary bs = deserializeTree (L.unpack as) (concat $ map byteToBits $ L.unpack $ ns) where
  (as, ns) = L.splitAt m $ L.drop 1 bs
  m = toEnum $ fromEnum $ (runGet getWord8) bs

compressWithLength :: Int64 -> L.ByteString -> L.ByteString
compressWithLength n bs = a `L.append` b `L.append` c  where
  a = U.toLazyByteString $ U.int64BE n
  b = serializeTreeBinary t
  c = encodeBinary m bs
  t = bytesToHuffmanTree bs
  m = toCodeTable t

compress :: L.ByteString -> L.ByteString
compress bs = compressWithLength (L.length bs) bs


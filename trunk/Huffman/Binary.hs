module Huffman.Binary (
  bytesToHuffmanTree,
  encodeBinary,
  decodeBinary 
) where

import Huffman
import qualified Data.Word
import qualified Data.Binary.BitBuilder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Bits
import Data.Char

type Byte = Data.Word.Word8

byteToBits :: Byte -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

bytesToHuffmanTree :: [Byte] -> HuffmanTree Byte
bytesToHuffmanTree = toTree [minBound::Byte .. maxBound::Byte]

encodeBinary :: M.Map Byte [Bool] -> [Byte] -> (Int, L.ByteString)
encodeBinary m s = (length b, packBits b) where
  packBits =
    B.toLazyByteString .
    foldl B.append B.empty .
    map B.singleton
  b = concat $ map (m M.!) s

decodeBinary :: HuffmanTree Byte -> Int -> [Byte] -> [Byte]
decodeBinary t n = (decode t) . take n . concat . (map byteToBits)


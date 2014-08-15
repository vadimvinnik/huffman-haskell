module Huffman.Tree (
  bytesToBitCodes
) where

import Huffman.Tree.Impl
import qualified Data.Word
import qualified Data.Binary.BitBuilder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Bits
import Data.Char

type Byte = Data.Word.Word8

bytesToBitCodes :: [Byte] -> M.Map Byte B.BitBuilder
bytesToBitCodes = itemsToCodes B.empty (appendBit False) (appendBit True) [minBound::Byte .. maxBound::Byte] where
  appendBit b = B.append $ B.singleton b

huffmanEncodeWith :: M.Map Byte B.BitBuilder -> [Byte] -> L.ByteString
huffmanEncodeWith m t = B.toLazyByteString $ foldl encodeByte B.empty t where
  encodeByte s b = B.append (m M.! b) s

huffmanEncode :: [Byte] -> L.ByteString
huffmanEncode s = huffmanEncodeWith (bytesToBitCodes s) s

huffmanEncodeString :: String -> L.ByteString
huffmanEncodeString = huffmanEncode . S.unpack . C.pack

byteToBits :: Byte -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

huffmanDecodeString :: HuffmanTree Byte -> S.ByteString -> String
huffmanDecodeString t = (map (chr . fromEnum)) . (decode t) .concat . (map byteToBits) . S.unpack
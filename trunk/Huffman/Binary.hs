module Huffman.Binary (
  toBinaryCodeTable
) where

import Huffman
import qualified Data.Word
import qualified Data.Binary.BitBuilder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Bits
import Data.Char

type Byte = Data.Word.Word8

stringToBytes :: String -> [Byte]
stringToBytes = S.unpack . C.pack

bytesToString :: [Byte] -> String
bytesToString = C.unpack . S.pack

bytesToHuffmanTree :: [Byte] -> HuffmanTree Byte
bytesToHuffmanTree = toTree [minBound::Byte .. maxBound::Byte]

toBinaryCodeTable :: HuffmanTree Byte -> M.Map Byte B.BitBuilder
toBinaryCodeTable = toCodeTable B.empty (appendBit False) (appendBit True) where
  appendBit b = B.append $ B.singleton b

encodeBinaryWith :: M.Map Byte B.BitBuilder -> [Byte] -> L.ByteString
encodeBinaryWith m t = B.toLazyByteString $ foldl encodeByte B.empty t where
  encodeByte s b = B.append (m M.! b) s

encodeBinary :: [Byte] -> L.ByteString
encodeBinary s = encodeBinaryWith (toBinaryCodeTable $ bytesToHuffmanTree s) s

encodeStringToBinary :: String -> L.ByteString
encodeStringToBinary = encodeBinary . stringToBytes

byteToBits :: Byte -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

decodeBinaryWith :: HuffmanTree Byte -> S.ByteString -> String
decodeBinaryWith t = bytesToString . (decode t) .concat . (map byteToBits) . S.unpack
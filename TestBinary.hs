import Huffman
import Huffman.Binary
import qualified Data.Word
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Char

type Byte = Data.Word.Word8

stringToBytes :: String -> [Byte]
stringToBytes = map (toEnum . ord)

bytesToString :: [Byte] -> String
bytesToString = map (chr . fromEnum)

test :: String -> ([Byte], String, Float)
test s = (e, d, 1.0 - ((fromIntegral $ length e) / (fromIntegral $ length s))) where
  b      = stringToBytes s
  t      = bytesToHuffmanTree b
  m      = toCodeTable t
  (n, c) = encodeBinary m b
  e      = L.unpack c
  d      = bytesToString $ decodeBinary t n e


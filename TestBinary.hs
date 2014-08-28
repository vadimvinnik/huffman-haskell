import Huffman
import Huffman.Binary
import Huffman.Show
import Huffman.Eq
import qualified Data.Word
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Char

type Byte = Data.Word.Word8

stringToBytes :: String -> L.ByteString
stringToBytes = L.pack . map (toEnum . ord)

bytesToString :: L.ByteString -> String
bytesToString = map (chr . fromEnum) . L.unpack 

test :: String -> (L.ByteString, String, Float)
test s = (e, d, 1.0 - ((fromIntegral $ L.length e) / (fromIntegral $ length s))) where
  b = stringToBytes s
  t = bytesToHuffmanTree b
  m = toCodeTable t
  e = encodeBinary m b
  d = bytesToString $ decodeBinary t (L.length b) e

testTreeSerialization :: String -> Bool
testTreeSerialization s = (t == d) where
  b = stringToBytes s
  t = bytesToHuffmanTree b
  e = serializeTreeBinary t
  d = deserializeTreeBinary e


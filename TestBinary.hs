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

test :: String -> (String, Float)
test s = (t, 1.0 - ((fromIntegral $ L.length e) / (fromIntegral $ length s))) where
  b = stringToBytes s
  e = compress b
  d = decompress e
  t = bytesToString d


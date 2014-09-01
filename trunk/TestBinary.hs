import Huffman.Binary
import qualified Data.Word
import qualified Data.ByteString.Lazy as L
import Data.Char

type Byte = Data.Word.Word8

stringToBytes :: String -> L.ByteString
stringToBytes = L.pack . map (toEnum . ord)

bytesToString :: L.ByteString -> String
bytesToString = map (chr . fromEnum) . L.unpack 

restore = bytesToString . decompress . compress . stringToBytes

test s = s == restore s


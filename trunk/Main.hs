import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as L
import Huffman.Binary

main = do
  (m:i:o:[]) <- getArgs
  p <- openBinaryFile i ReadMode
  q <- openBinaryFile o WriteMode
  n <- hFileSize p
  s <- L.hGetContents p
  L.hPut q $ (f m n) s
  hClose q
  hClose p
  where
    f "-c" n = compressWithLength (toEnum $ fromEnum n)
    f "-d" _ = decompress

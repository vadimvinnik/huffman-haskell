import System.IO
import System.Environment
import Data.Binary.Get
import Data.Array.IO
import Huffman
import Huffman.Binary
import Huffman.Show
import Data.Binary.Put
import Histogram
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

compressFile :: Handle -> Handle -> IO ()
compressFile i o = do
  n <- hFileSize i
  L.hPut o $ runPut $ putWord64be $ toEnum $ fromEnum n
  b <- L.hGetContents i
  let s = L.unpack b
  a <- makeHistogram s
  l <- getAssocs a
  let h = M.fromList l
  let c = compress h s 
  L.hPut o c
  
decompressFile :: Handle -> Handle -> IO ()
decompressFile i o = do
  s <- L.hGetContents i
  let t = decompress s
  L.hPut o t

main = do
  (m:i:o:[]) <- getArgs
  p <- openBinaryFile i ReadMode
  q <- openBinaryFile o WriteMode
  (process m) p q
  hClose q
  hClose p
  where
    process "-c" = compressFile
    process "-d" = decompressFile
    process _ = error "Usage: (-c|-d) infile outfile"

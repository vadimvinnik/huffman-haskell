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
  bs0 <- L.hGetContents i
  print $ take 16 $ L.unpack bs0
  let (n, bs1, _)  = runGetState getWord64be bs0 0
  print $ take 16 $ L.unpack bs1
  let (m, bs2, _)  = runGetState getWord8 bs1 0
  print $ take 16 $ L.unpack bs2
  print n
  print m
  let (bs3, bs4)   = L.splitAt (toEnum $ fromEnum m + 1) bs2
  let bs5          = concat $ map byteToBits $ L.unpack bs4
  let (t, bs6)     = deserializeTree (L.unpack bs3) bs5
  print t
  let c = L.pack $ take (toEnum $ fromEnum n) $ decode t bs6
  L.hPut o c

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

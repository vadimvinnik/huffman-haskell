import System.IO
import System.Environment
import Data.Binary.Get
import Data.Array.IO
import Data.Word
import Huffman
import Huffman.Binary
import Huffman.Show
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary.BitBuilder as B
import qualified Data.Map as M

updateHistogram :: IOArray Word8 Int -> Word8 -> IO ()
updateHistogram h x = do
  c <- readArray h x
  let d = c+1
  seq d writeArray h x d

makeHistogram :: [Word8] -> IO (Histogram Word8)
makeHistogram s = do
  a <- newArray (0,255) 0
  mapM_ (updateHistogram a) s
  l <- getAssocs a
  let h = M.fromList l
  return h

compressFile :: Handle -> Handle -> IO ()
compressFile i o = do
  n <- hFileSize i
  L.hPut o $ runPut $ putWord64be $ fromIntegral n
  b <- L.hGetContents i
  let s = L.unpack b
  h <- makeHistogram s
  let t = histogramToTree h
  let m = treeToCodeTable t
  let l = treeLeaves t
  L.hPut o $ runPut $ putWord8 $ fromIntegral $ subtract 1 $ length l
  L.hPut o $ L.pack l
  let v =  (treeStructure t) ++ (encode m s)
  L.hPut o $ B.toLazyByteString $ packBits v
  
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

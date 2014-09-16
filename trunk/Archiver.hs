import System.IO
import System.Environment
import Data.Binary.Get
import Data.Binary.Put
import Data.Array.IO
import Data.Bits
import Data.Word
import Huffman
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
  
byteToBits :: Word8 -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

packBits :: [Bool] -> B.BitBuilder
packBits = foldl B.append B.empty . map B.singleton

decompress :: L.ByteString -> L.ByteString
decompress bs0 = L.pack $ take (fromIntegral n) $ decode t bs6
  where
    (n, bs1, _)  = runGetState getWord64be bs0 0
    (m, bs2, _)  = runGetState getWord8 bs1 0
    (bs3, bs4)   = L.splitAt ((fromIntegral m) + 1) bs2
    bs5          = concat $ map byteToBits $ L.unpack bs4
    (t, bs6)     = restoreTree (L.unpack bs3) bs5

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

--
-- Huffman archiver
--
-- (c) Vadim Vinnik, 2014
--

import Huffman

import System.IO
import Control.Exception      (bracket)
import System.Environment     (getArgs)
import Data.Binary.Get        (getWord8, getWord64be, runGetState)
import Data.Binary.Put        (putWord8, putWord64be, runPut)
import Data.Array.IO          (IOArray, newArray, readArray, writeArray, getAssocs)
import Data.Bits              (testBit)
import Data.Word              (Word8, Word64)
import Data.Map               (fromList)
import Data.Binary.BitBuilder (BitBuilder, append, empty, singleton, toLazyByteString)
import Data.ByteString.Lazy   (hPut, unpack, pack)
import qualified Data.ByteString.Lazy as L (hGetContents, splitAt)

main = do
  (m:i:o:[]) <- getArgs
  bracket
    (openBinaryFile i ReadMode)
    (\h -> hClose h)
    (\h -> processInput m o h)

processInput m o i =
  bracket
    (openBinaryFile o WriteMode)
    (\h -> hClose h)
    (\h -> (process m) i h)

process "-c" = compressFile
process "-d" = decompressFile
process _ = error "Usage: (-c|-d) infile outfile"

updateHistogram :: IOArray Word8 Word64 -> Word8 -> IO ()
updateHistogram h x = do
  c <- readArray h x
  let d = c+1
  seq d writeArray h x d

makeHistogram :: [Word8] -> IO (Histogram Word8)
makeHistogram s = do
  a <- newArray (0,255) 0
  mapM_ (updateHistogram a) s
  l <- getAssocs a
  let h = fromList l
  return h

byteToBits :: Word8 -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

packBits :: [Bool] -> BitBuilder
packBits = foldl append empty . map singleton

compressFile :: Handle -> Handle -> IO ()
compressFile i o = do
  n <- hFileSize i
  hPut o $ runPut $ putWord64be $ fromIntegral n
  b <- L.hGetContents i
  let s = unpack b
  h <- makeHistogram s
  let t = histogramToTree h
  let m = treeToCodeTable t
  let l = treeLeaves t
  hPut o $ runPut $ putWord8 $ fromIntegral $ subtract 1 $ length l
  hPut o $ pack l
  let v =  (treeStructure t) ++ (encode m s)
  hPut o $ toLazyByteString $ packBits v
  
decompressFile :: Handle -> Handle -> IO ()
decompressFile i o = do
  s0 <- L.hGetContents i
  let (n, s1, _)  = runGetState getWord64be s0 0
  let (m, s2, _)  = runGetState getWord8 s1 0
  let (s3, s4)    = L.splitAt ((fromIntegral m) + 1) s2
  let s5          = concat $ map byteToBits $ unpack s4
  let (t, s6)     = restoreTree (unpack s3) s5
  let d           = pack $ take (fromIntegral n) $ decode t s6
  hPut o d


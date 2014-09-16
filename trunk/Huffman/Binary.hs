module Huffman.Binary (
  compress,
  byteToBits,
  decompress
) where

import Huffman
import qualified Data.Word
import qualified Data.Binary.BitBuilder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Bits
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import GHC.Int

type Byte = Data.Word.Word8

(+++) = B.append

byteToBits :: Byte -> [Bool]
byteToBits b = map (testBit b) $ reverse [0..7]

packBits :: [Bool] -> B.BitBuilder
packBits = foldl B.append B.empty . map B.singleton

serializeTreeToBits :: Tree Byte -> B.BitBuilder
serializeTreeToBits t =
  (B.fromLazyByteString $ runPut $ putWord8 $ fromIntegral $ (length as) - 1)
  +++
  (B.fromLazyByteString $ L.pack as)
  +++
  (packBits ns)
    where
      (as, ns) = serializeTree t

compress :: Histogram Byte -> [Byte] -> L.ByteString
compress h s = B.toLazyByteString $ (serializeTreeToBits t) +++ (packBits $ encode m s)
  where
    t = histogramToTree h
    m = treeToCodeTable t

decompress :: L.ByteString -> L.ByteString
decompress bs0 = L.pack $ take (fromIntegral n) $ decode t bs6
  where
    (n, bs1, _)  = runGetState getWord64be bs0 0
    (m, bs2, _)  = runGetState getWord8 bs1 0
    (bs3, bs4)   = L.splitAt ((fromIntegral m) + 1) bs2
    bs5          = concat $ map byteToBits $ L.unpack bs4
    (t, bs6)     = deserializeTree (L.unpack bs3) bs5


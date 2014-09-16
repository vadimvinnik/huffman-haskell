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
  (B.fromLazyByteString $ runPut $ putWord8 $ toEnum $ fromEnum $ (length as) - 1)
  +++
  (B.fromLazyByteString $ L.pack as)
  +++
  (packBits ns)
    where
      (as, ns) = serializeTree t

encodeToBits :: M.Map Byte [Bool] -> [Byte] -> B.BitBuilder
encodeToBits m = packBits . encode m

compress :: Histogram Byte -> [Byte] -> L.ByteString
compress h s = B.toLazyByteString $ b +++ c
  where
    b = serializeTreeToBits t
    c = encodeToBits m s
    t = histogramToTree h
    m = treeToCodeTable t

decompress :: L.ByteString -> L.ByteString
decompress bs0 = L.pack $ take (toEnum $ fromEnum n) $ decode t bs6
  where
    (n, bs1, _)  = runGetState getWord64be bs0 0
    (m, bs2, _)  = runGetState getWord8 bs1 0
    (bs3, bs4)   = L.splitAt ((toEnum $ fromEnum m) + 1) bs2
    bs5          = concat $ map byteToBits $ L.unpack bs4
    (t, bs6)     = deserializeTree (L.unpack bs3) bs5


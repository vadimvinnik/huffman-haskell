module Histogram (
  makeHistogram
) where

import qualified Data.ByteString.Lazy as L
import Data.Array.IO
import Data.Word

updateHistogram :: IOArray Word8 Int -> Word8 -> IO ()
updateHistogram h x = do
  c <- readArray h x
  let d = c+1
  seq d writeArray h x d

makeHistogram :: [Word8] -> IO (IOArray Word8 Int)
makeHistogram s = do
  g <- newArray (0,255) 0
  mapM_ (updateHistogram g) s
  return g


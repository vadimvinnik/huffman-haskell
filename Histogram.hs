import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as L
import Data.Array.IO
import Data.Word

type Byte = Data.Word.Word8

updateHistogram :: IOArray Byte Int -> Byte -> IO ()
updateHistogram h x = do
  c <- readArray h x
  let d = c+1
  seq d writeArray h x d

makeHistogram s = do
  g <- newArray (0,255) 0
  mapM_ (updateHistogram g) s
  return g

main = do
  (i:[]) <- getArgs
  p <- openBinaryFile i ReadMode
  b <- L.hGetContents p
  let s = L.unpack b
  g <- makeHistogram s
  hClose p
  l <- getAssocs g
  print l

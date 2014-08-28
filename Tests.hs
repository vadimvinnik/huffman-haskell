--
-- A test application for Huffman encoding
--
-- Only tests the worker functions representing binary
-- codes as strings of 0s and 1s for readability.
--
-- (c) Vadim Vinnik, 2014
--

import Huffman
import Huffman.Show
import qualified Data.Map as M

texts = [
  "too hot to hoot",
  "a quick brown fox jumps over the lazy dog",
  "this is just a short test string",
  "Haskell is a computer programming language. In particular, it is a polymorphically statically typed, lazy, purely functional language, quite different from most other programming languages. The language is named for Haskell Brooks Curry, whose work in mathematical logic serves as a foundation for functional languages. Haskell is based on the lambda calculus, hence the lambda we use as a logo."]

decodeWithStrings :: HuffmanTree Char -> String -> String
decodeWithStrings t = decode t . map (=='1')

data TestDetails = TestDetails {
  input   :: String,
  tree    :: HuffmanTree Char,
  codes   :: M.Map Char [Bool],
  encoded :: [Bool],
  decoded :: String
} deriving (Show)
  
testHuffman :: String -> TestDetails
testHuffman s = TestDetails {
    input = s,
    tree = t,
    codes = m,
    encoded = e,
    decoded = d
  } where
  d = decode t e
  e = encode m s
  m = toCodeTable t
  t = toTree "" s 

isPassed :: TestDetails -> Bool
isPassed x = (input x) == (decoded x)

main = do
  mapM printTest texts where
  printTest s =
    let t = testHuffman s in
    if isPassed t
      then do
        putStrLn "Passed"
      else do
        putStrLn "Failed"
        print t


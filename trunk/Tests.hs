import Huffman.Tree.Impl

import qualified Data.Map as M

texts = [
  "too hot to hoot",
  "a quick brown fox jumps over the lazy dog",
  "this is just a short test string",
  "Haskell is a computer programming language. In particular, it is a polymorphically statically typed, lazy, purely functional language, quite different from most other programming languages. The language is named for Haskell Brooks Curry, whose work in mathematical logic serves as a foundation for functional languages. Haskell is based on the lambda calculus, hence the lambda we use as a logo."]

treeToCodes :: HuffmanTree Char -> M.Map Char String
treeToCodes = freqTreeToCodes "" ('0':) ('1':)

huffmanEncodeWith :: M.Map Char String -> String -> String
huffmanEncodeWith m = concat . (map (m M.!))

huffmanDecodeString :: HuffmanTree Char -> String -> String
huffmanDecodeString t = decode t . map (=='1')

testHuffman :: String -> Bool
testHuffman s = d == s where
  d = huffmanDecodeString t e
  e = huffmanEncodeWith m s
  m = treeToCodes t
  t = itemsToFreqTree "" s 

main = do
  putStrLn $ show $ map testHuffman texts

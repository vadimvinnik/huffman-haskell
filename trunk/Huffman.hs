--
-- Worker functions for building frequency tables, Huffman trees and code tables
--
-- (c) Vadim Vinnik, 2014
--

module Huffman (
  HuffmanTree,
  Tree (Leaf, Fork),
  toTree,
  toCodeTable,
  encode,
  decode
) where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Function

-- Build frequency table from a list of items

type FreqTable a = M.Map a Int

toFreqTable :: Ord a => [a] -> [a] -> FreqTable a
toFreqTable v t = M.fromListWith (+) ((toPairs 0 v) ++ (toPairs 1 t)) where
  toPairs x = L.map (flip (,) x)

-- Build Huffman tree from a frequency table

data Tree a = Fork (Tree a) (Tree a) | Leaf a
type FreqTree a = Tree (a, Int)
type FreqForest a = [FreqTree a]
type HuffmanTree a = Tree a

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf $ f x
mapTree f (Fork p q) = Fork (mapTree f p) (mapTree f q)

weight :: FreqTree a -> Int
weight (Leaf (_, n)) = n
weight (Fork x y) = (weight x) + (weight y)

freqForestToTree :: Ord a => FreqForest a -> FreqTree a
freqForestToTree [] = error "the frequency forest is empty"
freqForestToTree [x] = x
freqForestToTree (x: (y: s)) = freqForestToTree $ L.insertBy (compare `on` weight) (Fork x y) s

toTree :: Ord a => [a] -> [a] -> HuffmanTree a
toTree v =
  mapTree fst .
  freqForestToTree .
  (L.sortBy (compare `on` weight)) .
  (map Leaf) .
  M.toList .
  toFreqTable v

-- Build Huffman code table from the tree

toCodeTable :: Ord a => HuffmanTree a -> M.Map a [Bool]
toCodeTable (Leaf x) = M.singleton x []
toCodeTable (Fork u v) = M.union (subtreeToCodes False u) (subtreeToCodes True v) where
    subtreeToCodes h t = M.map (h:) (toCodeTable t)

-- Encode

encode :: Ord a => M.Map a [Bool] -> [a] -> [Bool]
encode m = concat . map (m M.!)

-- Decode

decode :: HuffmanTree a -> [Bool] -> [a]
decode t s = decodeEngine t s where
  decodeEngine (Leaf c) [] = [c]
  decodeEngine (Leaf c) bs = c : decodeEngine t bs
  decodeEngine (Fork p q) (b:bs) = decodeEngine (if b then q else p) bs

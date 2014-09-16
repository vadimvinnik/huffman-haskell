--
-- Worker functions for Huffman encoding and decoding
--
-- (c) Vadim Vinnik, 2014
--

module Huffman (
  Histogram,
  Tree (Leaf, Fork),
  CodeTable,
  histogramToTree,
  treeToCodeTable,
  serializeTree,
  deserializeTree,
  encode,
  decode
) where

import Data.Map (Map, keys, fromListWith, singleton, union, (!))
import qualified Data.Map as M
import Data.List (sortBy, insertBy)
import Data.Function

type Histogram a = Map a Int
type CodeTable a = Map a [Bool]
data Tree a = Fork (Tree a) (Tree a) | Leaf a

histogramToTree :: Ord a => Histogram a -> Tree a
histogramToTree h =
  forestToTree $
  sortBy (compare `on` weight) $
  map Leaf $
  keys h
  where
    forestToTree [x] = x
    forestToTree (x: (y: s)) = forestToTree $ insertBy (compare `on` weight) (Fork x y) s
    weight (Leaf c) = h ! c
    weight (Fork x y) = (weight x) + (weight y)

-- serialize node structure: F opens a fork, T denotes a leaf;
-- e.g. tree ((a b) (c d)) turns into (a b c d, F F T T F T T)
serializeTree :: Tree a -> ([a], [Bool])
serializeTree t = (leaves t [], structure t [])
  where
    leaves (Leaf x) ys = x:ys
    leaves (Fork u v) ys = leaves u (leaves v ys)
    structure (Leaf _) bs = True:bs
    structure (Fork u v) bs = False:(structure u (structure v bs))

deserializeTree :: [a] -> [Bool] -> (Tree a, [Bool])
deserializeTree cs bs = (t, bs')
  where
    (t, _, bs') = deserializeTree' cs bs
    deserializeTree' (c:cs) (True:bs) = (Leaf c, cs, bs)
    deserializeTree' cs0 (False:bs0) = (Fork u v, cs2, bs2)
      where
        (u, cs1, bs1) = deserializeTree' cs0 bs0
        (v, cs2, bs2) = deserializeTree' cs1 bs1

treeToCodeTable :: Ord a => Tree a -> CodeTable a
treeToCodeTable (Leaf x) = singleton x []
treeToCodeTable (Fork u v) = union (subtreeToCodes False u) (subtreeToCodes True v)
  where
    subtreeToCodes h t = M.map (h:) (treeToCodeTable t)

encode :: Ord a => CodeTable a -> [a] -> [Bool]
encode m = concat . map (m !)

decode :: Tree a -> [Bool] -> [a]
decode t s = decodeEngine t s
  where
    decodeEngine (Leaf c) [] = [c]
    decodeEngine (Leaf c) bs = c : decodeEngine t bs
    decodeEngine (Fork p q) (b:bs) = decodeEngine (if b then q else p) bs


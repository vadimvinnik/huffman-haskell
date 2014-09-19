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
  treeLeaves,
  treeStructure,
  restoreTree,
  encode,
  decode
) where

import Data.Function (on)
import Data.List     (sortBy, insertBy)
import Data.Map      (Map, keys, fromListWith, singleton, union, (!))
import qualified Data.Map as M (map)

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

treeLeaves :: Tree a -> [a]
treeLeaves = treeLeaves' []
  where
    treeLeaves' ys (Leaf x) = x:ys
    treeLeaves' ys (Fork u v) = treeLeaves' (treeLeaves' ys v) u

-- serialize node structure: F opens a fork, T denotes a leaf;
-- e.g. tree ((a b) (c d)) turns into (F F T T F T T)
treeStructure :: Tree a -> [Bool]
treeStructure = treeStructure' []
  where
    treeStructure' bs (Leaf _) = True:bs
    treeStructure' bs (Fork u v) = False:(treeStructure' (treeStructure' bs v) u)

restoreTree :: [a] -> [Bool] -> (Tree a, [Bool])
restoreTree cs bs = (t, bs')
  where
    (t, _, bs') = restoreTree' cs bs
    restoreTree' (c:cs) (True:bs) = (Leaf c, cs, bs)
    restoreTree' cs0 (False:bs0) = (Fork u v, cs2, bs2)
      where
        (u, cs1, bs1) = restoreTree' cs0 bs0
        (v, cs2, bs2) = restoreTree' cs1 bs1

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


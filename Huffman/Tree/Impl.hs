--
-- Worker functions for building frequency tables, Huffman trees and code tables
--

module Huffman.Tree.Impl (
  itemsToFreqTable,
  freqTableToForest,
  freqForestToTree,
  itemsToFreqTree,
  freqTreeToCodes,
  itemsToCodes 
) where

import qualified Data.Map as M
import qualified Data.List as L

--
-- Build frequency table from a list of items
--

type FreqTable a = M.Map a Int

itemsToFreqTable :: Ord a => [a] -> [a] -> FreqTable a
itemsToFreqTable v t = M.fromListWith (+) ((toPairs 0 v) ++ (toPairs 1 t)) where
  toPairs x = L.map (flip (,) x)

--
-- Build Huffman tree from a frequency table
--

data Tree a = Fork (Tree a) (Tree a) | Leaf a
type FreqTree a = Tree (Int, a)
type FreqForest a = [FreqTree a]

instance (Show a) => Show (Tree a) where
  show = showIndent "" where
    showIndent p (Leaf x) = (show x) ++ "\n"
    showIndent p (Fork u v) =
      "*\n" ++ p ++ "|--" ++
      (showIndent (p ++ "|  ") u) ++
      p ++ "|\n" ++ p ++ "+--" ++
      (showIndent (p ++ "   ") v)

compareFreqTrees :: FreqTree a -> FreqTree a -> Ordering
compareFreqTrees x y = compare (weight x) (weight y)

weight :: FreqTree a -> Int
weight (Leaf (n, _)) = n
weight (Fork x y) = (weight x) + (weight y)

freqTableToForest :: Ord a => FreqTable a -> FreqForest a
freqTableToForest = (L.sortBy compareFreqTrees) . (map pairToFreqItem) . M.toList where
    pairToFreqItem (x, n) = (Leaf (n, x))

freqForestToTree :: Ord a => FreqForest a -> FreqTree a
freqForestToTree [] = error "the frequency forest is empty"
freqForestToTree [x] = x
freqForestToTree (x: (y: s)) = freqForestToTree $ L.insertBy compareFreqTrees (Fork x y) s

itemsToFreqTree :: Ord a => [a] -> [a] -> FreqTree a
itemsToFreqTree v = freqForestToTree . freqTableToForest . itemsToFreqTable v

--
-- Build Huffman code table from the tree
--

freqTreeToCodes :: Ord a => b -> (b -> b) -> (b -> b) -> FreqTree a -> M.Map a b
freqTreeToCodes s _ _ (Leaf (_, x)) = M.singleton x s
freqTreeToCodes s f g (Fork u v) = M.union (subtreeToCodes f u) (subtreeToCodes g v) where
    subtreeToCodes h t = M.map h (freqTreeToCodes s f g t)

itemsToCodes :: Ord a => b -> (b -> b) -> (b -> b) -> [a] -> [a] -> M.Map a b
itemsToCodes s f g v = (freqTreeToCodes s f g) . (itemsToFreqTree v)


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
itemsToFreqTable v = foldl addToFreqTable (M.fromList $ map itemToPair v) where
    itemToPair x = (x, 0)
    addToFreqTable t x = updateFreqTable t (M.lookup x t) where
        updateFreqTable t Nothing = M.insert x 1 t
        updateFreqTable t (Just n) = M.insert x (n+1) t


--
-- Build Huffman tree from a frequency table
--

data FreqTree a = FreqNode (FreqTree a) (FreqTree a) | FreqItem Int a
type FreqForest a = [FreqTree a]

instance Show a => Show (FreqTree a) where
  show = showIndent "" where
    showIndent p (FreqItem n c) = (show c) ++ " " ++ (show n) ++ "\n"
    showIndent p t@(FreqNode u v) =
      (show $ nodeFrequency t) ++
      "\n" ++ p ++ "|--" ++
      (showIndent (p ++ "|  ") u) ++
      p ++ "|\n" ++ p ++ "+--" ++
      (showIndent (p ++ "   ") v)

compareFreqTrees :: FreqTree a -> FreqTree a -> Ordering
compareFreqTrees x y = compare (nodeFrequency x) (nodeFrequency y)

nodeFrequency :: FreqTree a -> Int
nodeFrequency (FreqItem n _) = n
nodeFrequency (FreqNode x y) = (nodeFrequency x) + (nodeFrequency y)

freqTableToForest :: Ord a => FreqTable a -> FreqForest a
freqTableToForest = (L.sortBy compareFreqTrees) . (map pairToFreqItem) . M.toList where
    pairToFreqItem (x, n) = (FreqItem n x)

freqForestToTree :: Ord a => FreqForest a -> FreqTree a
freqForestToTree [] = error "the frequency forest is empty"
freqForestToTree [x] = x
freqForestToTree (x: (y: s)) = freqForestToTree $ L.insertBy compareFreqTrees (FreqNode x y) s

itemsToFreqTree :: Ord a => [a] -> [a] -> FreqTree a
itemsToFreqTree v = freqForestToTree . freqTableToForest . itemsToFreqTable v

--
-- Build Huffman code table from the tree
--

freqTreeToCodes :: Ord a => b -> (b -> b) -> (b -> b) -> FreqTree a -> M.Map a b
freqTreeToCodes s _ _ (FreqItem _ x) = M.singleton x s
freqTreeToCodes s f g (FreqNode u v) = M.union (subtreeToCodes f u) (subtreeToCodes g v) where
    subtreeToCodes h t = M.map h (freqTreeToCodes s f g t)

itemsToCodes :: Ord a => b -> (b -> b) -> (b -> b) -> [a] -> [a] -> M.Map a b
itemsToCodes s f g v = (freqTreeToCodes s f g) . (itemsToFreqTree v)


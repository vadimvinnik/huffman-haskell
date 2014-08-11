module Huffman.Tree (
  itemsToCodes 
) where

import Huffman.Tree.Impl
import qualified Data.Map as M

itemsToCodes :: Ord a => b -> (b -> b) -> (b -> b) -> [a] -> [a] -> M.Map a b
itemsToCodes s f g v = (freqTreeToCodes s f g) . (itemsToFreqTree v)


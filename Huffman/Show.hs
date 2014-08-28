module Huffman.Show (
  show
) where

import Huffman

instance (Show a) => Show (Tree a) where
  show = showIndent "" where
    showIndent p (Leaf x) = (show x) ++ "\n"
    showIndent p (Fork u v) =
      "*\n" ++ p ++ "|--" ++
      (showIndent (p ++ "|  ") u) ++
      p ++ "|\n" ++ p ++ "+--" ++
      (showIndent (p ++ "   ") v)


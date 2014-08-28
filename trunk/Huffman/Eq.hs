module Huffman.Eq (
  (==)
) where

import Huffman

instance (Eq a) => Eq (Tree a) where
  (Leaf x) == (Leaf y) = (x == y)
  (Fork p q) == (Fork u v) = (p == u && q == v)
  _ == _ = False


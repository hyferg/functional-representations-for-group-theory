import Data.Tree
import Data.List
import MathObj.LaurentPolynomial as LP

-- class BinaryTree a where
--   singleton :: (Ord b) => b -> c
-- -- treeInsert :: (Ord b) => b -> a -> a
-- -- treeElem :: (Ord b) => b -> a -> Bool
--
--
-- data LRTree a = EmptyTree | Node a (LRTree a) (LRTree a) deriving (Show, Read, Eq)
--
-- instance BinaryTree (LRTree a) where
--   singleton a = Node a EmptyTree EmptyTree

-- treeInsert x EmptyTree = singleton x
-- treeInsert x (Node a left right)
--   | x == a = Node x left right
--   | x < a = Node a (treeInsert x left) right
--   | x > a = Node a left (treeInsert x right)
--
-- treeElem x EmptyTree = False
-- treeElem x (Node a left right)
--   | x == a = True
--   | x < a = treeElem x left
--   | x > a = treeElem x right


-- data PolyTree a = Leaf | Poly a [PolyTree a] deriving (Show, Read, Eq)


-- tree folding demonstration
-- vertical is *
-- horizontal is +

tree = Node "a"
  [Node "b" [],
   Node "c" [
      Node "d" [],
      Node "e" []
      ]
  ]

handleNode x xs
  | addition == [] = x
  | otherwise = x ++ "*(" ++ addListElem xs ++ ")"
  where addListElem xs = intercalate "+" xs
        addition = addListElem xs

folded = foldTree handleNode tree


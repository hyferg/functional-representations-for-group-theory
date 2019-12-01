module FlatGraph where
import Data.Map.Strict as Map hiding (take)

data EdgeType = R | B | G deriving (Show)
type Nidx = Int
type Eidx = Int
data Node = Node Nidx [Edge]
data Edge = Edge Eidx [Node] EdgeType

class FlatGraph g where
  getNode_ :: Nidx -> g -> Maybe Node
  getEdge_ :: Eidx -> g -> Maybe Edge
  freeEdgeIndicesOf_ :: Int -> g -> [Eidx]
  freeNodeIndicesOf_ :: Int -> g -> [Nidx]
  insertNodes_ :: [Node] -> g -> Maybe g
  insertEdges_ :: [Edge] -> g -> Maybe g
  allNodes_ :: g -> [Node]
  allEdges_ :: g -> [Edge]

f :: (FlatGraph g) => g -> Maybe g
f g = let
  eIndices = 3 `freeEdgeIndicesOf_` g
  nIndices = 3 `freeNodeIndicesOf_` g

  e1 = Edge (eIndices !! 0) [nk,ni] R
  e2 = Edge (eIndices !! 1) [ni,nj] B
  e3 = Edge (eIndices !! 2) [nj,nk] G

  ni = Node (nIndices !! 0) [e1, e2]
  nj = Node (nIndices !! 1) [e2, e3]
  nk = Node (nIndices !! 2) [e3, e1]

  in return g >>= insertNodes_ [ni,nj,nk] >>= insertEdges_ [e1,e2,e3]

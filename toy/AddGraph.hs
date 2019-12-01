module AddGraph where
import FlatGraph

addRing :: (FlatGraph g) => g -> Maybe g
addRing g = let
  eIDXs = 3 `freeEdgeIndicesOf_` g
  nIDXs = 3 `freeNodeIndicesOf_` g

  e1 = Edge (eIDXs !! 0) [n1, n2] G
  e2 = Edge (eIDXs !! 1) [n2, n3] G
  e3 = Edge (eIDXs !! 2) [n3, n1] G

  n1 = Node (nIDXs !! 0) [e1, e3]
  n2 = Node (nIDXs !! 1) [e1, e2]
  n3 = Node (nIDXs !! 2) [e2, e3]

  in return g
  >>= insertNodes_ [n1,n2,n3]
  >>= insertEdges_ [e1,e2,e3]

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

  in return g
  >>= insertNodes_ [ni,nj,nk]
  >>= insertEdges_ [e1,e2,e3]

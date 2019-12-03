module AddGraph where
import FlatGraph

addSunP1 :: (FlatGraph g) => g -> Maybe g
addSunP1 g = let
  eI = 5 `freeEdgeIndicesOf_` g
  nI = 6 `freeNodeIndicesOf_` g

  e15 = Edge (eI !! 0) [n1, n5] U
  e25 = Edge (eI !! 1) [n2, n5] D
  e36 = Edge (eI !! 2) [n3, n6] D
  e46 = Edge (eI !! 3) [n4, n6] U
  e56 = Edge (eI !! 4) [n5, n6] G

  n1 = Node (nI !! 0) [e15]
  n2 = Node (nI !! 1) [e25]
  n3 = Node (nI !! 2) [e36]
  n4 = Node (nI !! 3) [e46]
  n5 = Node (nI !! 4) [e15, e25, e56]
  n6 = Node (nI !! 5) [e56, e46, e36]

  in return g >>= work_ [
  InsertE [e15, e25, e36, e46, e56],
  InsertN [n1, n2, n3, n4, n5, n6] ]

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

  in return g >>= work_ [
  InsertN [n1,n2,n3],
  InsertE [e1,e2,e3] ]

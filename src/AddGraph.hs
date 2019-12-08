module AddGraph where
import FlatGraph

pill :: (FlatGraph g) => g -> Maybe g
pill g = let
  eL = 3 `freeEdgeLabelsOf_` g
  nL = 2 `freeNodeLabelsOf_` g

  eij = Edge (eL !! 0) [ni, nj] D
  eg  = Edge (eL !! 1) [ni, nj] G
  eji = Edge (eL !! 2) [nj, ni] D

  ni = Node (nL !! 0) [eij, eg, eji]
  nj = Node (nL !! 1) [eji, eg, eij]

  in return g >>= work_ [
  InsertN [ni, nj], InsertE [eij, eg, eji]
                        ]


{-
(ni) __eij__ (nj) __ejk__ (nk)
-}
tripleGluon :: (FlatGraph g) => g -> Maybe g
tripleGluon g = let
  eL = 3 `freeEdgeLabelsOf_` g
  nL = 4 `freeNodeLabelsOf_` g

  nc = Node (nL !! 3) [eci, ecj, eck]
  ni = Node (nL !! 0) [eci]
  nj = Node (nL !! 1) [ecj]
  nk = Node (nL !! 2) [eck]

  eci = Edge (eL !! 0) [nc,ni] G
  ecj = Edge (eL !! 1) [nc,nj] G
  eck = Edge (eL !! 2) [nc,nk] G

  in return g >>= work_ [
  InsertN [ni,nj,nk,nc],
  InsertE [eci,ecj,eck] ]

{-
(ni) __eij__ (nj) __ejk__ (nk)
-}
line :: (FlatGraph g) => g -> Maybe g
line g = let
  eL = 2 `freeEdgeLabelsOf_` g
  nL = 3 `freeNodeLabelsOf_` g

  ni = Node (nL !! 0) [eij]
  nj = Node (nL !! 1) [eij,ejk]
  nk = Node (nL !! 2) [ejk]

  eij = Edge (eL !! 0) [ni,nj] G
  ejk = Edge (eL !! 1) [nj,nk] G

  in return g >>= work_ [
  InsertN [ni,nj,nk],
  InsertE [eij,ejk] ]


handBag :: (FlatGraph g) => g -> Maybe g
handBag g = let
  eI = 3 `freeEdgeLabelsOf_` g
  nI = 2 `freeNodeLabelsOf_` g

  n1 = Node (nI !! 0) [eg, ej, ei]
  n2 = Node (nI !! 1) [ei, ej, eg]

  eg = Edge (eI !! 0) [n1, n2] G
  ei = Edge (eI !! 1) [n1, n2] U
  ej = Edge (eI !! 2) [n1, n2] D

  in return g >>= work_ [
  InsertN [n1, n2],
  InsertE [eg, ei, ej] ]

addSunP1 :: (FlatGraph g) => g -> Maybe g
addSunP1 g = let
  eI = 5 `freeEdgeLabelsOf_` g
  nI = 6 `freeNodeLabelsOf_` g

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
  eIDXs = 3 `freeEdgeLabelsOf_` g
  nIDXs = 3 `freeNodeLabelsOf_` g

  e1 = Edge (eIDXs !! 0) [n1, n2] G
  e2 = Edge (eIDXs !! 1) [n2, n3] G
  e3 = Edge (eIDXs !! 2) [n3, n1] G

  n1 = Node (nIDXs !! 0) [e1, e3]
  n2 = Node (nIDXs !! 1) [e1, e2]
  n3 = Node (nIDXs !! 2) [e2, e3]

  in return g >>= work_ [
  InsertN [n1,n2,n3],
  InsertE [e1,e2,e3] ]

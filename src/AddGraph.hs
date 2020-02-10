module AddGraph where
import FlatGraph

{-
stockGraph :: (FlatGraph g) => g -> Maybe g
stockGraph g = let
  eL = _  `freeEdgeLabelsOf_` g
  nL = _  `freeNodeLabelsOf_` g

  e = Edge (eL !! _)  [] _
  n = Node (nL !! _) []

  in return g >>= work_ [InsertE [], InsertN]
-}

vector :: (FlatGraph g) => g -> Maybe g
vector g = let
  eL = 1  `freeEdgeLabelsOf_` g
  nL = 2  `freeNodeLabelsOf_` g

  e =  Edge (eL !! 0) [n0,n1] U
  n0 = Node (nL !! 0) [e]
  n1 = Node (nL !! 1) [e]

  in return g >>= work_ [InsertE [e], InsertN [n0, n1]]

half6jLL :: (FlatGraph g) => g -> Maybe g
half6jLL g = let
  eL = 4  `freeEdgeLabelsOf_` g
  nL = 4  `freeNodeLabelsOf_` g

  eg  = Edge (eL !! 0)  [n0,n1] G
  e01 = Edge (eL !! 1)  [n0,n1] D
  e0i = Edge (eL !! 2)  [n0,ni] U
  e1j = Edge (eL !! 3)  [n1,nj] D

  n0 = Node (nL !! 0) [e0i,e01,eg]
  n1 = Node (nL !! 1) [e1j,eg,e01]
  ni = Node (nL !! 2) [e0i]
  nj = Node (nL !! 3) [e1j]

  in return g >>= work_ [
  InsertE [
      eg,
      e01,
      e0i,
      e1j ],
  InsertN [
      n0,
      n1,
      ni,
      nj ]
  ]


half6jLR :: (FlatGraph g) => g -> Maybe g
half6jLR g = let
  eL = 4  `freeEdgeLabelsOf_` g
  nL = 4  `freeNodeLabelsOf_` g

  eg  = Edge (eL !! 0)  [n0,n1] G
  e01 = Edge (eL !! 1)  [n0,n1] D
  e0i = Edge (eL !! 2)  [n0,ni] U
  e1j = Edge (eL !! 3)  [n1,nj] D

  n0 = Node (nL !! 0) [e0i,e01,eg]
  n1 = Node (nL !! 1) [eg,e01,e1j]
  ni = Node (nL !! 2) [e0i]
  nj = Node (nL !! 3) [e1j]

  in return g >>= work_ [
  InsertE [
      eg,
      e01,
      e0i,
      e1j ],
  InsertN [
      n0,
      n1,
      ni,
      nj ]
  ]

flippedSunP1 :: (FlatGraph g) => g -> Maybe g
flippedSunP1 g = let
  eI = 5 `freeEdgeLabelsOf_` g
  nI = 6 `freeNodeLabelsOf_` g

  e15 = Edge (eI !! 0) [n1, n5] D
  e25 = Edge (eI !! 1) [n2, n5] U
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

peace6j :: (FlatGraph g) => g -> Maybe g
peace6j g = let
  eL = 6  `freeEdgeLabelsOf_` g
  nL = 4  `freeNodeLabelsOf_` g

  e13 = Edge (eL !! 0)  [n1,n3] D
  e32 = Edge (eL !! 1)  [n3,n2] D
  e21 = Edge (eL !! 2)  [n2,n1] D
  e01 = Edge (eL !! 3)  [n0,n1] G
  e03 = Edge (eL !! 4)  [n0,n3] G
  e02 = Edge (eL !! 5)  [n0,n2] G

  n0 = Node (nL !! 0) [e01,e03,e02]
  n1 = Node (nL !! 1) [e13,e01,e21]
  n3 = Node (nL !! 3) [e32,e03,e13]
  n2 = Node (nL !! 2) [e21,e02,e32]

  in return g >>= work_ [
  InsertN [
      n0,
      n1,
      n2,
      n3 ],
  InsertE [
      e13,
      e32,
      e21,
      e01,
      e03,
      e02 ]
  ]



dumbell :: (FlatGraph g) => g -> Maybe g
dumbell g = let
  eL = 3  `freeEdgeLabelsOf_` g
  nL = 2  `freeNodeLabelsOf_` g

  n1n2 = Edge (eL !! 0)  [n1, n2] G
  n11 = Edge (eL !! 1)  [n1, n1] D
  n22 = Edge (eL !! 2)  [n2, n2] D

  n1 = Node (nL !! 0) [n11, n11, n1n2]
  n2 = Node (nL !! 1) [n1n2, n22, n22]

  in return g >>= work_ [
  InsertE [
      n1n2,
      n11,
      n22 ],
  InsertN [
      n1,
      n2 ]]

twoCasimir :: (FlatGraph g) => g -> Maybe g
twoCasimir g = let
  eL = 6  `freeEdgeLabelsOf_` g
  nL = 4  `freeNodeLabelsOf_` g

  n21n11 = Edge (eL !! 0)  [n21, n11] D
  n11n21 = Edge (eL !! 1)  [n11, n21] D
  n12n22 = Edge (eL !! 2)  [n12, n22] D
  n22n12 = Edge (eL !! 3)  [n22, n12] D
  n11n12 = Edge (eL !! 4)  [n11, n12] G
  n21n22 = Edge (eL !! 5)  [n21, n22] G

  n11 = Node (nL !! 0) [n11n21, n21n11, n11n12]
  n21 = Node (nL !! 1) [n21n11, n11n21, n21n22]
  n12 = Node (nL !! 2) [n11n12, n12n22, n22n12]
  n22 = Node (nL !! 3) [n21n22, n22n12, n12n22]

  in return g >>= work_ [
  InsertE [
      n21n11,
      n11n21,
      n12n22,
      n22n12,
      n11n12,
      n21n22],
    InsertN [
      n11,
      n21,
      n12,
      n22]
  ]

oneCasimir :: (FlatGraph g) => g -> Maybe g
oneCasimir g = let
  eL = 3 `freeEdgeLabelsOf_` g
  nL = 2  `freeNodeLabelsOf_` g

  eg  = Edge (eL !! 0)  [n1, n2] G
  e11 = Edge (eL !! 1) [n1, n1] D
  e22 = Edge (eL !! 2) [n2, n2] D

  n1 = Node (nL !! 0) [e11, e11, eg]
  n2 = Node (nL !! 1) [eg, e22, e22]

  in return g >>= work_ [InsertN [n1, n2], InsertE [eg, e11, e22]]


fourCasimir :: (FlatGraph g) => g -> Maybe g
fourCasimir g = let

  eL = 12 `freeEdgeLabelsOf_` g
  nL = 8  `freeNodeLabelsOf_` g

  n11 = Node (nL !! 0) [n41n11, n11n21, e1]
  n21 = Node (nL !! 1) [n11n21, n21n31, e2]
  n31 = Node (nL !! 2) [n21n31, n31n41, e3]
  n41 = Node (nL !! 3) [n31n41, n41n11, e4]

  n42 = Node (nL !! 7) [e4, n12n42, n42n32]
  n32 = Node (nL !! 6) [e3, n42n32, n32n22]
  n22 = Node (nL !! 5) [e2, n32n22, n22n12]
  n12 = Node (nL !! 4) [e1, n22n12, n12n42]

  n11n21 = Edge (eL !! 0) [n11,n21] D
  n21n31 = Edge (eL !! 1) [n21,n31] D
  n31n41 = Edge (eL !! 2) [n31,n41] D
  n41n11 = Edge (eL !! 3) [n41,n11] D

  n42n32 = Edge (eL !! 4) [n42,n32] D
  n32n22 = Edge (eL !! 5) [n32,n22] D
  n22n12 = Edge (eL !! 6) [n22,n12] D
  n12n42 = Edge (eL !! 7) [n12,n42] D

  e1 = Edge (eL !! 8) [n11, n12] G
  e2 = Edge (eL !! 9) [n21, n22] G
  e3 = Edge (eL !! 10) [n31, n32] G
  e4 = Edge (eL !! 11) [n41, n42] G

  in return g >>= work_ [
  InsertN [
  n11,
  n21,
  n31,
  n41,
  n42,
  n32,
  n22,
  n12
          ],
  InsertE [
  n11n21,
  n21n31,
  n31n41,
  n41n11,
  n42n32,
  n32n22,
  n22n12,
  n12n42,
  e1,
  e2,
  e3,
  e4
      ]]

selfGluon :: (FlatGraph g) => g -> Maybe g
selfGluon g = let
  eL = 4 `freeEdgeLabelsOf_` g
  nL = 3 `freeNodeLabelsOf_` g

  eij = Edge (eL !! 0) [ni, nj] G
  ejk = Edge (eL !! 1) [nj, nk] G
  ekj = Edge (eL !! 2) [nk, nj] G
  ekl = Edge (eL !! 3) [nk, nl] G

  ni = Node (nL !! 0) [eij]
  nj = Node (nL !! 1) [ejk, ejk]
  nk = Node (nL !! 2) [ejk, ekj]
  nl = Node (nL !! 3) [ekl]

  in return g >>= work_ [
  InsertN [ni, nj, nk, nl], InsertE [eij, ejk, ekj, ekl] ]

pill :: (FlatGraph g) => g -> Maybe g
pill g = let
  eL = 3 `freeEdgeLabelsOf_` g
  nL = 2 `freeNodeLabelsOf_` g

  eij = Edge (eL !! 0) [ni,nj] D
  eg  = Edge (eL !! 1) [ni,nj] G
  eji = Edge (eL !! 2) [nj,ni] D

  ni = Node (nL !! 0) [eg,eij,eji]
  nj = Node (nL !! 1) [eij,eg,eji]

  in return g >>= work_ [
  InsertN [ni, nj], InsertE [eij, eg, eji] ]

{-
(ni) __eij__ (nj) __ejk__ (nk)
-}
tripleGluon :: (FlatGraph g) => g -> Maybe g
tripleGluon g = let
  eL = 3 `freeEdgeLabelsOf_` g
  nL = 4 `freeNodeLabelsOf_` g

  nc = Node (nL !! 3) [eci,ecj,eck]
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


threeCasimir :: (FlatGraph g) => g -> Maybe g
threeCasimir g = let

  eL = 100  `freeEdgeLabelsOf_` g
  nL = 100  `freeNodeLabelsOf_` g

  n11 = Node (nL !! 0) [n31n11, n11n21, e1]
  n21 = Node (nL !! 1) [n11n21, n21n31, e2]
  n31 = Node (nL !! 2) [n21n31, n31n11, e3]

  n32 = Node (nL !! 6) [e3, n12n32, n32n22]
  n22 = Node (nL !! 5) [e2, n32n22, n22n12]
  n12 = Node (nL !! 4) [e1, n22n12, n12n32]

  n11n21 = Edge (eL !! 0) [n11,n21] D
  n21n31 = Edge (eL !! 1) [n21,n31] D

  n32n22 = Edge (eL !! 2) [n32,n22] D
  n22n12 = Edge (eL !! 3) [n22,n12] D

  n12n32 = Edge (eL !! 4) [n12, n32] D
  n31n11 = Edge (eL !! 5) [n31, n11] D

  e1 = Edge (eL !! 8) [n11, n12] G
  e2 = Edge (eL !! 9) [n21, n22] G
  e3 = Edge (eL !! 10) [n31, n32] G

  in return g >>= work_ [
  InsertN [
  n11,
  n21,
  n31,
  n32,
  n22,
  n12
          ],
  InsertE [
  n11n21,
  n21n31,
  n32n22,
  n22n12,
  e1,
  e2,
  e3,
  n12n32,
  n31n11
      ]]


{-
-- https://hackage.haskell.org/package/numeric-prelude
import MathObj.Polynomial as Poly
import MathObj.Polynomial.Core as Core
https://hackage.haskell.org/package/besout-0.2.0.1/docs/Bezout.html
https://hackage.haskell.org/package/poly-0.3.3.0/docs/Data-Poly.html
-}
import Data.Poly as Poly
import Data.Ratio as Ratio
import Data.Euclidean as Eu
import Prelude hiding (product)

import GraphData
import GraphRecursive
import Helper
import Graphs
import Rules

g = emptyGraph

sunAdj = do
  let
    nL = 6  `freeNodeLabelsOf` g
    eL = 5  `freeEdgeLabelsOf` g

    n0 = N (nL !! 0) [e01,e20,e03]
    n1 = N (nL !! 1) [e01]
    n2 = N (nL !! 2) [e20]
    n3 = N (nL !! 3) [e03,e34,e53]
    n4 = N (nL !! 4) [e34]
    n5 = N (nL !! 5) [e53]

    e01 = E (eL !! 0) [n0,n1] D
    e20 = E (eL !! 1) [n2,n0] D
    e03 = E (eL !! 2) [n0,n3] G
    e34 = E (eL !! 3) [n3,n4] D
    e53 = E (eL !! 4) [n5,n3] D

  g' <- return g >>= product([n0,n1,n2,n3,n4,n5],[e01,e20,e03,e34,e53])
  return (e03, g')

out = do
  (e, g') <- sunAdj
  return g' >>= identity e

anEdge = do
  let
    nL = 2  `freeNodeLabelsOf` g
    eL = 1  `freeEdgeLabelsOf` g
    n0 = N (nL !! 0) [e]
    n1 = N (nL !! 1) [e]
    e = E (eL !! 0) [n0,n1] G
  g' <- return g >>= product([n0,n1],[e])
  return (e, g')

aLoop = do
  let
    nL = 1  `freeNodeLabelsOf` g
    eL = 1  `freeEdgeLabelsOf` g
    n = N (nL !! 0) [e,e]
    e = E (eL !! 0) [n,n] G
  g' <- return g >>= product([n],[e])
  return (e, g')

aChain = do
  let
    nL = 4  `freeNodeLabelsOf` g
    eL = 3  `freeEdgeLabelsOf` g

    n0 = N (nL !! 0) [e01]
    n1 = N (nL !! 1) [e01,e12]
    n2 = N (nL !! 2) [e12,e23]
    n3 = N (nL !! 3) [e23]

    e01 = E (eL !! 0) [n0,n1] G
    e12 = E (eL !! 1) [n1,n2] G
    e23 = E (eL !! 2) [n2,n3] G

  g' <- return g >>= product([n0,n1,n2,n3],[e01,e12,e23])
  return (e12, g')


dangle = do
  let
    nL = 4  `freeNodeLabelsOf` g
    eL = 2  `freeEdgeLabelsOf` g

    -- base
    n0 = N (nL !! 0) [e01]
    n1 = N (nL !! 1) [e01]
    n2 = N (nL !! 2) [e23]
    n3 = N (nL !! 3) [e23]

    e01 = E (eL !! 0) [n0, n1] G
    e23 = E (eL !! 1) [n2, n3] G

  g' <- product ([n0,n1,n2,n3],[e01,e23]) g
  (n', g'') <- mergeNodes [n1,n2] g'
  (_, g''') <- removeNode n' g''

  return g'''

chain = do
  let
    nL = 3  `freeNodeLabelsOf` g
    eL = 2  `freeEdgeLabelsOf` g

    -- base
    n0 = N (nL !! 0) [e01]
    n1 = N (nL !! 1) [e01, e12]
    n2 = N (nL !! 2) [e12]

    e01 = E (eL !! 0) [n0, n1] D
    e12 = E (eL !! 1) [n1, n2] U

  g' <- product ([n0,n1,n2],[e01,e12]) g
  (_, g'') <- removeNode n1 g'

  return g''

generator = do
  let
    eL = 3  `freeEdgeLabelsOf` g
    nL = 4  `freeNodeLabelsOf` g

    -- base
    n0 = N (nL !! 0) [e01]
    n1 = N (nL !! 1) [e01]
    e01 = E (eL !! 0) [n0, n1] G

    -- insert
    n1' = N (nL !! 1) [Ghost, e13, e12]
    n2' = N (nL !! 2) [e12]
    n3' = N (nL !! 3) [e13]

    e13 = E (eL !! 1) [n1',n3'] U
    e12 = E (eL !! 2) [n1',n2'] D

  g' <- product ([n0,n1],[e01]) g
  g'' <- product ([n1',n2',n3'],[e12,e13]) g'
  return g''

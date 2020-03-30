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

g = emptyGraph
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

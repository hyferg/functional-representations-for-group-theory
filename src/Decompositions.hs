module Decompositions where
import FlatGraph

import MathObj.LaurentPolynomial as LP
type Poly = LP.T Int
data VectorSpace poly g = VS poly g deriving (Show)

zero = fromCoeffs [0]
plusOne = fromCoeffs [0]
plusN = fromCoeffs [0]
minusOne = fromCoeffs [0]
minusOverN = fromCoeffs [0]

-- RULES --

{-
sunP1Decomposition :: (FlatGraph g) => Edge -> g -> Either [VectorSpace Poly g] g
sunP1Decomposition emn g
  | Just lhs <- sunP1LHS emn g
  , Just rhs <- sunP1RHS emn g
  = Left [VS plusOne lhs, VS minusOverN rhs]
  | otherwise = Right g
-}

gluonExpansion :: (FlatGraph g) => Node -> g -> Either [VectorSpace Poly g] g
gluonExpansion node graph
  | Just lhs <- gluonExpansionGraph "anticlock" node graph
  , Just rhs <- gluonExpansionGraph "clock" node graph
  = Left [VS plusOne lhs, VS minusOne rhs]
  | otherwise = Right graph

-- GRAPH OPERATIONS --

--sunP1LHS :: (FlatGraph g) => Edge -> g -> Maybe g
sunP1LHS emn g
  | (Edge _ [nm, nn] G) <- emn
  , chiralEq nm nn
  = do
      g' <- return g >>= work_ [RemoveE [emn]]
      ([nmBot, nmTop], g'')  <- safeSplit_ nm g'
      ([nnTop, nnBot], g''') <- safeSplit_ nn g''
      --return g''' >>= work_ [Merge [(nmTop, nnTop)]]
      return g''' >>= work_ [Merge [(nmTop, nnTop), (nmBot, nnBot)]]
      --return g''' >>= work_ [Merge [(nmBot, nnBot)]]

sunP1RHS :: (FlatGraph g) => Edge -> g -> Maybe g
sunP1RHS emn g
  | (Edge _ [nm, nn] G) <- emn
  , chiralEq nm nn
  = return g >>= work_ [RemoveE [emn]]

gluonExpansionGraph :: (FlatGraph g) => String -> Node -> g -> Maybe g
gluonExpansionGraph rotation nc g
  | (Node _ [eic, ejc, ekc]) <- nc
  , (Edge _ _ G) <- eic
  , (Edge _ _ G) <- ejc
  , (Edge _ _ G) <- ekc
  , rotation == "clock" || rotation == "anticlock"
  = do
      ([n1, n2, n3], g') <- split_ nc g
      (Node n1L [e1i]) <- Just n1
      (Node n2L [e2j]) <- Just n2
      (Node n3L [e3k]) <- Just n3
      let
        eL = 3 `freeEdgeLabelsOf_` g
        eType = if rotation == "anticlock" then D else U

        e12 = Edge (eL !! 0) [n1, n2] eType
        e23 = Edge (eL !! 1) [n2, n3] eType
        e31 = Edge (eL !! 2) [n3, n1] eType

        n1' = Node n1L [e1i, e12, e31]
        n2' = Node n2L [e12, e2j, e23]
        n3' = Node n3L [e31, e23, e3k]

        in return g' >>= work_ [
        InsertE [e12, e23, e31],
        Swap [(n1, n1'),(n2, n2'),(n3, n3') ]]
  | otherwise = Nothing


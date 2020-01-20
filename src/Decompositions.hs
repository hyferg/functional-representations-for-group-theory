module Decompositions where
import FlatGraph

import MathObj.LaurentPolynomial as LP
type Poly = LP.T Int
data VectorSpace poly g = VS poly g deriving (Show)

zero :: Poly
zero = fromCoeffs []

plusOne :: Poly
plusOne = fromCoeffs [1]

plusN :: Poly
plusN = fromCoeffs [0,1]

minusOne :: Poly
minusOne = fromShiftCoeffs (-1) [1]

minusOverN :: Poly
minusOverN = fromShiftCoeffs (-2) [1]

-- RULES --

-- TODO add shrinkchain to the merged nodes
sunP1Rule :: (FlatGraph g) =>
  (Edge, VectorSpace Poly g) ->
  Maybe (Poly, [VectorSpace Poly g])
sunP1Rule (emn, (VS poly g))
  | Just lhs <- sunP1LHS emn g
  , Just rhs <- sunP1RHS emn g
  = Just (poly, [VS plusOne lhs, VS minusOverN rhs])
  | otherwise = Nothing

gggRule :: (FlatGraph g) =>
  (Node, VectorSpace Poly g) ->
  Maybe (Poly, [VectorSpace Poly g])
gggRule (node, (VS poly g))
  | Just lhs <- gluonExpansionGraph "anticlock" node g
  , Just rhs <- gluonExpansionGraph "clock" node g
  = Just (poly, [VS plusOne lhs, VS minusOne rhs])
  | otherwise = Nothing

shrinkChainRule :: (FlatGraph g) =>
  (Node, VectorSpace Poly g) ->
  Maybe (Poly, [VectorSpace Poly g])
shrinkChainRule (node, (VS poly g))
  | Just g' <- shrinkChain node g
  = Just (poly, [VS plusOne g'])
  | otherwise = Nothing

tadpoleRule :: (FlatGraph g) =>
  (Node, VectorSpace Poly g) ->
  Maybe (Poly, [VectorSpace Poly g])
tadpoleRule (node, _)
  | True <- tadpole node
  = Just (zero, [])
  | otherwise = Nothing

-- TODO delete loop and multiply graph by scalar
loopRule :: (FlatGraph g) =>
  (Node, VectorSpace Poly g) ->
  Maybe (Poly, [VectorSpace Poly g])
loopRule (node, VS poly g)
  | Just g' <- loop node g
  = Just (poly, [VS plusN g'])
  | otherwise = Nothing

-- GRAPH OPERATIONS --

-- quark loop
loop :: (FlatGraph g) => Node -> g -> Maybe g
loop node g
  | (Node _ [e1, e2]) <- node
  , e1 =@= e2
  , edgeType e1 == U || edgeType e1 == D
  = return g >>= work_ [DeleteN [node], DeleteE [e1, e2]]
  | otherwise = Nothing

isTadpole :: Edge -> Edge -> Edge -> Bool
isTadpole e1 e2 e3
  | e1 =@= e2
  , edgeType e1 == U || edgeType e2 == D
  , edgeType e3 == G
  = True
  | otherwise = False

tadpole :: Node -> Bool
tadpole node
  | Node _ [e1, e2, e3] <- node
  , isTadpole e1 e2 e3 ||
    isTadpole e3 e1 e2 ||
    isTadpole e2 e3 e1
  = True
  | otherwise = False


-- (ni) __eij__ (nj) __ejk__ (nk)
-- (ni) __eik__ (nk)
-- for directed type equality of eij, ejk
shrinkChain :: (FlatGraph g) => Node -> g -> Maybe g
shrinkChain nj g
  | (Node _ [eij', ejk']) <- nj
  , eji <- orientEdge nj eij'
  , ejk <- orientEdge nj ejk'
  , eji /=@= ejk
  , invert eji =-= ejk
  = do
      ni <- otherNode eji nj
      nk <- otherNode ejk nj
      eikLabel <- return $ head $ 1 `freeEdgeLabelsOf_` g
      eik  <- return $ Edge eikLabel [ni, nk] (edgeType ejk)
      return g >>= swapChain_ (ni, eji, nj, ejk, nk) eik
  | otherwise = Nothing

sunP1LHS :: (FlatGraph g) => Edge -> g -> Maybe g
sunP1LHS emn g
  | (Edge _ [nm, nn] G) <- emn
  , chiralEq nm nn
  = do
      g' <- return g >>= work_ [RemoveE [emn]]
      ([nmBot, nmTop], g'')  <- safeSplit_ nm g'
      ([nnTop, nnBot], g''') <- safeSplit_ nn g''
      return g''' >>= work_ [Merge [(nmTop, nnTop), (nmBot, nnBot)]]
  | otherwise = Nothing

sunP1RHS :: (FlatGraph g) => Edge -> g -> Maybe g
sunP1RHS emn g
  | (Edge _ [nm, nn] G) <- emn
  , chiralEq nm nn
  = return g >>= work_ [RemoveE [emn]]
  | otherwise = Nothing

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


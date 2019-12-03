module Decompositions where
import FlatGraph

import MathObj.LaurentPolynomial as LP
type Poly = LP.T Int
data VectorSpace poly g = VS poly g deriving (Show)

zero = fromCoeffs [0]
plusOne = fromCoeffs [0]
minusOne = fromCoeffs [0]
minusOverN = fromCoeffs [0]

-- FULL DECOMPOSITIONS --

sunP1 :: (FlatGraph g) => Edge -> g -> Either [VectorSpace Poly g] g
sunP1 emn g
  | Just (lhs, rhs) <- sunP1Split emn g = Left [
      VS plusOne lhs, VS minusOverN rhs]
  | otherwise = Right g

gluonExpansion :: (FlatGraph g) => Node -> g -> Either [VectorSpace Poly g] g
gluonExpansion nl g
  | Just lhs <- gluonExpansionHalf "clock" nl g
  , Just rhs <- gluonExpansionHalf "anticlock" nl g
  = Left [VS plusOne lhs, VS minusOne rhs]
  | otherwise = Right g

tadPole :: (FlatGraph g) => Node -> g -> Either [VectorSpace Poly g] g
tadPole ni g
  | Just g' <- tadPoleSubGraph ni g = Left [VS zero g']
  | otherwise = Right g

killChain :: (FlatGraph g) => Node -> g -> Either [VectorSpace Poly g] g
killChain nj g
  | Just g' <- killChainSubGraph nj g = Left [VS plusOne g']
  | otherwise = Right g

-- GRAPH DECOMPOSITIONS --

sunP1Split :: (FlatGraph g) => Edge -> g -> Maybe (g, g)
sunP1Split emn g
  | (Edge _ [nm, nn] G) <- emn
  , isClock nm && isClock nn || isAntiClock nm && isAntiClock nn
  = do
      (emi, emk) <- otherEdges (oriented nm) emn
      (enl, enj) <- otherEdges (oriented nn) emn

      ni <- otherNode emi nm
      nk <- otherNode emk nm
      nj <- otherNode enj nn
      nl <- otherNode enl nn

      let
        eI = 2 `freeEdgeIndicesOf_` g

        remove = [
          RemoveN [nm, nn],
          RemoveE [emi, emk, enj, enl, emn] ]

        -- lhs edges (+1)
        eji = Edge (eI !! 0) [nj, ni] $ edgeType emi
        ekl = Edge (eI !! 1) [nk, nl] $ edgeType enl
        lhs = return g >>= work_ (
          remove ++ [
              InsertE [eji, ekl],
              SubEdge [(ni, emi, eji),
                       (nj, enj, eji),
                       (nk, emk, ekl),
                       (nl, enl, ekl) ]])

        -- rhs edges (-1/n)
        eki = Edge (eI !! 0) [nk, ni] $ edgeType emi
        ejl = Edge (eI !! 1) [nj, nl] $ edgeType enl
        rhs = return g >>= work_ (
          remove ++ [
              InsertE [eki, ejl],
              SubEdge [(ni, emi, eki),
                       (nk, emk, eki),
                       (nj, enj, ejl),
                       (nl, enl, ejl) ]])

        in do
          l <- lhs
          r <- rhs
          return (l, r)
  | otherwise = Nothing


--TODO add gluon check
tadPoleSubGraph :: (FlatGraph g) => Node -> g -> Maybe g
tadPoleSubGraph ni g
  | (Node _ [eij, eik]) <- ni
  , (Edge _ _ eijType) <- eij
  , (Edge _ _ eikType) <- eik
  , eijType == eikType && eijType == U || eijType == D
  , eij == eik = Just g
  | otherwise = Nothing

gluonExpansionHalf :: (FlatGraph g) => String -> Node -> g -> Maybe g
gluonExpansionHalf rotation nl g
  | (Node _ [eil, ejl, ekl]) <- nl
  , (Edge eilIDX _ G) <- eil
  , (Edge ejlIDX _ G) <- ejl
  , (Edge eklIDX _ G) <- ekl
  , rotation == "clock" || rotation == "anticlock"
  = do
      ni <- otherNode eil nl
      nj <- otherNode ejl nl
      nk <- otherNode ekl nl
      let
        eI = 3 `freeEdgeIndicesOf_` g
        nI = 3 `freeNodeIndicesOf_` g

        ei1 = Edge eilIDX [ni, n1] G
        ej2 = Edge ejlIDX [nj, n2] G
        ek3 = Edge eklIDX [nk, n3] G

        n1 = Node (nI !! 0) [ei1, e12, e31]
        n2 = Node (nI !! 1) [e12, ej2, e23]
        n3 = Node (nI !! 2) [e23, ek3, e31]

        qType = if rotation == "clock" then D else U

        e12 = Edge (eI !! 0) [n1, n2] qType
        e23 = Edge (eI !! 1) [n2, n3] qType
        e31 = Edge (eI !! 2) [n3, n1] qType

        in return g >>= work_ [
        RemoveN [nl],
        Replace [(ejl, ej2), (eil, ei1), (ekl, ek3)],
        InsertE [e12, e23, e31, ei1, ej2, ek3],
        InsertN [n1, n2, n3] ]
  | otherwise = Nothing

{-
(ni) _eij_ (nj) _ejk_ (nk)
to
(ni)       _e1_       (nk)
where _eij_ , _ejk_ , _e1_ are G
-}
killChainSubGraph :: (FlatGraph g) => Node -> g -> Maybe g
killChainSubGraph nj g
  | (Node _ [eij, ejk]) <- nj
  , (Edge _ _ G) <- eij
  , (Edge _ _ G) <- ejk
  = do
      ni <- otherNode eij nj
      nk <- otherNode ejk nj
      g' <- let
        e1 = Edge (head $ 1 `freeEdgeIndicesOf_` g) [ni, nk] G
        in return g >>= work_ [
        InsertE [e1],
        RemoveN [nj],
        RemoveE [eij, ejk],
        SubEdge [ (ni, eij, e1),
                  (nk, ejk, e1) ]]
      return g'
  | otherwise = Nothing

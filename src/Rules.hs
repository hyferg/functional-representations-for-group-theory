module Rules (
  module GraphRecursive,
  Poly, VectorSpace(..), Decomposed, Scope(..),
  twistRule, gggRule, tadpoleRule,
  shrinkChainRule, loopRule, sunAdjRule, sonAdjRule, metricRule
             ) where
import GraphRecursive
import Poly

import MathObj.LaurentPolynomial as LP

--type Poly = LP.T Int

data VectorSpace g = VS Poly g  deriving (Show)

data Scope g = EdgeScope (Edge, VectorSpace g) | NodeScope (Node, VectorSpace g)
type Decomposed g = (Poly, [VectorSpace g])


-- EXPORTS --

twistRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
twistRule (EdgeScope (emn, (VS poly g)))
  | Just g' <- twist emn g
  = Just (poly, [VS minusOne g'])
  | otherwise = Nothing

-- TODO add shrinkchain to the merged nodes
sunAdjRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
sunAdjRule (EdgeScope (emn, (VS poly g)))
  | Just lhs <- identity emn g
  , Just rhs <- sunP1RHS emn g
  = Just (poly, [VS plusOne lhs, VS minusOverN rhs])
  | otherwise = Nothing

sonAdjRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
sonAdjRule (EdgeScope (emn, (VS poly g)))
  | Just lhs <- identity emn g
  , Just rhs <- cross emn g
  = Just (mul half poly, [VS plusOne lhs, VS minusOne rhs])
  | otherwise = Nothing

gggRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
gggRule (NodeScope (node, (VS poly g)))
  | Just lhs <- gluonExpansionGraph "anticlock" node g
  , Just rhs <- gluonExpansionGraph "clock" node g
  = Just (poly, [VS plusOne lhs, VS minusOne rhs])
  | otherwise = Nothing

shrinkChainRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
shrinkChainRule (NodeScope (node, (VS poly g)))
  | Just g' <- shrinkChain node g
  = Just (poly, [VS plusOne g'])
  | otherwise = Nothing

tadpoleRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
tadpoleRule (NodeScope (node, _))
  | True <- tadpole node
  = Just (zero, [])
  | otherwise = Nothing

-- TODO delete loop and multiply graph by scalar
loopRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
loopRule (NodeScope (node, VS poly g))
  | Just g' <- loop node g
  = Just (poly, [VS plusN g'])
  | otherwise = Nothing

metricRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
metricRule (EdgeScope (edge, VS poly g))
  | Just g' <- metricFlip edge g
  = Just (poly, [VS plusOne g'])
  | otherwise = Nothing


-- UTILS --

metricFlip :: (GraphRecursive g) => Edge -> g -> Maybe g
metricFlip eij g
  | (E _ [ni', nj'] eijType) <- eij
  , (N _ niEdges) <- oriented ni'
  , (N _ njEdges) <- oriented nj'
  , edgeTypes niEdges `elem` metrics && edgeTypes njEdges `elem` metrics
  = return g >>= work [UpdateEdgeType [(eij, invert eijType)]]
  | otherwise = Nothing
  where
    metrics = [[U, U], [D, D]]

-- quark loop
loop :: (GraphRecursive g) => Node -> g -> Maybe g
loop node g
  | (N _ [e1, e2]) <- node
  , e1 =@ e2
  , e1 `is` U || e1 `is` D
  = return g >>= work [DeleteN [node], DeleteE [e1, e2]]
  | otherwise = Nothing

isTadpole :: Edge -> Edge -> Edge -> Bool
isTadpole e1 e2 e3
  | e1 =@ e2
  , e1 `is` U || e2 `is` D
  , e3 `is` G
  = True
  | otherwise = False

tadpole :: Node -> Bool
tadpole node
  | N _ [e1, e2, e3] <- node
  , isTadpole e1 e2 e3 ||
    isTadpole e3 e1 e2 ||
    isTadpole e2 e3 e1
  = True
  | otherwise = False


-- (ni) __eij__ (nj) __ejk__ (nk)
-- (ni) __eik__ (nk)
-- for directed type equality of eij, ejk
shrinkChain :: (GraphRecursive g) => Node -> g -> Maybe g
shrinkChain nj g
  | (N _ [eij', ejk']) <- nj
  , eji <- orientEdge nj eij'
  , ejk <- orientEdge nj ejk'
  , eji /=@ ejk
  , rotate eji =~ ejk
  = do
      ni <- otherNode eji nj
      nk <- otherNode ejk nj
      eikLabel <- return $ head $ 1 `freeEdgeLabelsOf` g
      eik  <- return $ E eikLabel [ni, nk] (edgeType ejk)
      return g >>= swapChain (ni, eji, nj, ejk, nk) eik
  | otherwise = Nothing

--vectMatch :: EdgeType -> [Node] -> [Node]
--vectMatch targetType nodes = trace (show nodes) (vectMatch' targetType $ map oriented nodes)
vectMatch targetType nodes
  | length fNodes == 1 = Just $ head fNodes
-- | otherwise = Nothing
  where
    teq (N _ [E _ _ eType]) = eType == targetType
    fNodes = [n | n <- nodes, teq $ oriented n]


identity :: (GraphRecursive g) => Edge -> g -> Maybe g
identity emn g
  | (E _ [nm, nn] G) <- emn
  , chiralEq nm nn
  = do
      g' <- return g >>= work [RemoveE [emn]]
      (nms, g'')  <- safeSplit nm g'
      (nns, g''') <- safeSplit nn g''
      --(_,_) <- trace (show nms) safeSplit_ nn g''
      nnU <- vectMatch U nns
      nnD <- vectMatch D nns
      nmU <- vectMatch U nms
      nmD <- vectMatch D nms
      return g''' >>= work [Merge [(nnU, nmD), (nnD, nmU)]]
  | otherwise = Nothing

cross :: (GraphRecursive g) => Edge -> g -> Maybe g
cross emn g
  | (E _ [nm, nn] G) <- emn
  , chiralEq nm nn
  = do
      g' <- return g >>= work [RemoveE [emn]]
      (nms, g'')  <- safeSplit nm g'
      (nns, g''') <- safeSplit nn g''
      --(_,_) <- trace (show nms) safeSplit_ nn g''
      nnU <- vectMatch U nns
      nnD <- vectMatch D nns
      nmU <- vectMatch U nms
      nmD <- vectMatch D nms
      return g''' >>= work [Merge [(nnU, nmU), (nnD, nmD)]]
  | otherwise = Nothing


-- exchange generator indices
twist :: (GraphRecursive g) => Edge -> g -> Maybe g
twist emn g
  | (E _ [nm, nn] G) <- emn
  , antiChiralEq nm nn
  , (N nL edges) <- nm
  = do return g >>= work[Swap [(nm, (N nL (Prelude.reverse edges)))]]
  | otherwise = Nothing

sunP1RHS :: (GraphRecursive g) => Edge -> g -> Maybe g
sunP1RHS emn g
  | (E _ [nm, nn] G) <- emn
  , chiralEq nm nn
  = return g >>= work [RemoveE [emn]]
  | otherwise = Nothing

gluonExpansionGraph :: (GraphRecursive g) => String -> Node -> g -> Maybe g

--gluonExpansionGraph rotation nc g = trace (show nc) gluonExpansionGraph' rotation nc g
gluonExpansionGraph rotation nc g
  | (N _ [eic, ejc, ekc]) <- nc
  , (E _ _ G) <- eic
  , (E _ _ G) <- ejc
  , (E _ _ G) <- ekc
  , rotation == "clock" || rotation == "anticlock"
  = do
      ([n1, n2, n3], g') <- safeSplit nc g
      (N n1L [e1i]) <- Just n1
      (N n2L [e2j]) <- Just n2
      (N n3L [e3k]) <- Just n3
      let
        eL = 3 `freeEdgeLabelsOf` g
        eType = if rotation == "anticlock" then D else U

        e12 = E (eL !! 0) [n1, n2] eType
        e23 = E (eL !! 1) [n2, n3] eType
        e31 = E (eL !! 2) [n3, n1] eType

        n1' = N n1L [e1i, e12, e31]
        n2' = N n2L [e12, e2j, e23]
        n3' = N n3L [e31, e23, e3k]

        in return g' >>= work [
        InsertE [e12, e23, e31],
        Swap [(n1, n1'),(n2, n2'),(n3, n3') ]]
  | otherwise = Nothing

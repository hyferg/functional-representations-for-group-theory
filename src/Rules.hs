module Rules (
  module GraphRecursive,
  Poly, VectorSpace(..), Decomposed, Scope(..),
  sunAdjRule,
  sonAdjRule,
  loopRule,
  shrinkChainRule,
  tadpoleRule,
  gggRule,
  twistRule,
  metricRule
             ) where
import GraphRecursive
import Poly
import Helper
import Prelude hiding (product)

import MathObj.LaurentPolynomial as LP

--type Poly = LP.T Int

data VectorSpace g = VS Poly g  deriving (Show)

data Scope g = EdgeScope (Edge, VectorSpace g) | NodeScope (Node, VectorSpace g)
type Decomposed g = (Poly, [VectorSpace g])


-- NOTE EXPORTS --

-- TODO add shrinkchain to the merged nodes
-- check on chirality and type
sunAdjRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
sunAdjRule (EdgeScope (eij, (VS poly g)))
  | E _ [ni, nj] G <- eij
  , N _ nies <- ni
  , edgeTypes nies `elem` cyclePermute [D,U,G] ||
    edgeTypes nies `elem` cyclePermute [U,D,G]
  , ni =* nj
  , Just lhs <- identity eij g
  , Just rhs <- trace eij g
  = Just (poly, [VS plusOne lhs, VS minusOverN rhs])
  | otherwise = Nothing

sonAdjRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
sonAdjRule (EdgeScope (emn, (VS poly g)))
  | Just lhs <- identity emn g
  , Just rhs <- cross emn g
  = Just (mul half poly, [VS plusOne lhs, VS minusOne rhs])
  | otherwise = Nothing

loopRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
loopRule (NodeScope (node, VS poly g))
  | Just g' <- loop node g
  = Just (poly, [VS plusN g'])
  | otherwise = Nothing

shrinkChainRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
shrinkChainRule (NodeScope (node, (VS poly g)))
  | Just g' <- shrinkChain node g
  = Just (poly, [VS plusOne g'])
  | otherwise = Nothing

tadpoleRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
tadpoleRule (NodeScope (node, _))
  | True <- isNodeTadpole node
  = Just (zero, [])
  | otherwise = Nothing

gggRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
gggRule (NodeScope (node, (VS poly g)))
  | Just lhs <- gluonExpansion "anticlock" node g
  , Just rhs <- gluonExpansion "clock" node g
  = Just (poly, [VS plusOne lhs, VS minusOne rhs])
  | otherwise = Nothing

twistRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
twistRule (EdgeScope (eij, (VS poly g)))
  | (E _ [ni, nj] G) <- eij
  , ni =** nj
  , Just g' <- twist eij g
  = Just (poly, [VS minusOne g'])
  | otherwise = Nothing

metricRule :: (GraphRecursive g) => Scope g -> Maybe (Decomposed g)
metricRule (EdgeScope (edge, VS poly g))
  | Just g' <- metricFlip edge g
  = Just (poly, [VS plusOne g'])
  | otherwise = Nothing


-- NOTE decompositions

identity :: (GraphRecursive g) => Edge -> g -> Maybe g
identity eij g
  | E _ [ni, nj] G <- eij
  , N _ esi <- ni
  , N _ esj <- nj
  , 3 == length esi && length esi == length esj
  = do
      ((lAlpha,_,lBeta), g')  <- splitNodeCenterOn ni eij g
      ((rAlpha,_,rBeta), g'') <- splitNodeCenterOn ni eij g'

      return g'' >>=
        removeEdge eij >>= passGraph >>=
        mergeNodes [lBeta, rAlpha] >>= passGraph >>=
        mergeNodes [lAlpha, rBeta] >>= passGraph

cross :: (GraphRecursive g) => Edge -> g -> Maybe g
cross eij g
  | E _ [ni, nj] G <- eij
  , N _ esi <- ni
  , N _ esj <- nj
  , 3 == length esi && length esi == length esj
  = do
      ((lAlpha,_,lBeta), g')  <- splitNodeCenterOn ni eij g
      ((rAlpha,_,rBeta), g'') <- splitNodeCenterOn ni eij g'

      return g'' >>=
        removeEdge eij >>= passGraph >>=
        mergeNodes [lBeta, rBeta]   >>= passGraph >>=
        mergeNodes [lAlpha, rAlpha] >>= passGraph

trace :: (GraphRecursive g) => Edge -> g -> Maybe g
trace eij g
  | E _ [ni, nj] G <- eij
  , N _ esi <- ni
  , N _ esj <- nj
  , 3 == length esi && length esi == length esj
  = do
      ((lAlpha,_,lBeta), g')  <- splitNodeCenterOn ni eij g
      ((rAlpha,_,rBeta), g'') <- splitNodeCenterOn ni eij g'

      return g'' >>=
        removeEdge eij >>= passGraph >>=
        mergeNodes [lAlpha, lBeta] >>= passGraph >>=
        mergeNodes [rAlpha, rBeta] >>= passGraph

-- quark loop
loop :: (GraphRecursive g) => Node -> g -> Maybe g
loop node g
  | (N _ [e1, e2]) <- node
  , e1 =@ e2
  , e1 `isEdgeType` U || e1 `isEdgeType` D
  , Just(_, g') <- return g >>= removeEdge e1
  = Just g'
  | otherwise = Nothing

-- (ni) __eij__ (nj) __ejk__ (nk)
-- (ni) __eik__ (nk)
-- for directed type equality of eij, ejk
shrinkChain :: (GraphRecursive g) => Node -> g -> Maybe g
shrinkChain nj g
  | Just (_, g')<- return g >>= removeNode nj
  = Just g'
  | otherwise = Nothing

-- 'clock' or 'anticlock'
gluonExpansion :: (GraphRecursive g) =>  String -> Node -> g -> Maybe g
gluonExpansion rotation nc g
  | (N _ [eic, ejc, ekc]) <- nc
  , (E _ _ G) <- eic
  , (E _ _ G) <- ejc
  , (E _ _ G) <- ekc
  , rotation == "clock" || rotation == "anticlock"
  = do
      ([n1, n2, n3], g') <- splitNode nc g
      (N n1L _) <- return n1
      (N n2L _) <- return n2
      (N n3L _) <- return n3
      let
        eL = 3 `freeEdgeLabelsOf` g
        eType = if rotation == "anticlock" then D else U

        e12 = E (eL !! 0) [n1, n2] eType
        e23 = E (eL !! 1) [n2, n3] eType
        e31 = E (eL !! 2) [n3, n1] eType

        n1' = N n1L [Ghost, e12, e31]
        n2' = N n2L [e12, Ghost, e23]
        n3' = N n3L [e23, Ghost, e31]

        in return g' >>= product ([n1',n2',n3'],[e12,e23,e31])
  | otherwise = Nothing

-- exchange generator indices
twist :: (GraphRecursive g) => Edge -> g -> Maybe g
twist eij g
  | (E _ [ni, nj] G) <- eij
  = do
      ([n1,n2,n3], g') <- splitNode ni g
      (_, g'') <- return g' >>= mergeNodes [n3,n2,n1]
      return g''
  | otherwise = Nothing

metricFlip :: (GraphRecursive g) => Edge -> g -> Maybe g
metricFlip eij g
  | (E _ [ni', nj'] eijType) <- eij
  , (N _ niEdges) <- oriented ni'
  , (N _ njEdges) <- oriented nj'
  , edgeTypes niEdges `elem` metrics && edgeTypes njEdges `elem` metrics
  , Just (_, g') <- return g >>= updateEdgeType (eij, (invert eijType))
  = Just g'
  | otherwise = Nothing
  where
    metrics = [[U, U], [D, D]]


{- NOTE legacy rules

-- UTILS --

-- (ni) ->- (nj) to
-- (ni) -<- (nj)

-}



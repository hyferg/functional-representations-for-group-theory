module FlatGraph where
import MathObj.LaurentPolynomial as LP

data EdgeType = U | D | G deriving (Show, Eq)
type Nidx = Int
type Eidx = Int
data Node = Node Nidx [Edge]
data Edge = Edge Eidx [Node] EdgeType

data Operation = InsertE [Edge] | RemoveE [Edge] |
                 InsertN [Node] | RemoveN [Node] |
                 SubEdge [(Node, Edge, Edge)] |
                 Replace [(Edge, Edge)]

type Poly = LP.T Int
data VectorSpace poly g = VS poly g

zero = fromCoeffs [0]
plusOne = fromCoeffs [0]
minusOne = fromCoeffs [0]
minusOverN = fromCoeffs [0]

class FlatGraph g where
  getNode_ :: Nidx -> g -> Maybe Node
  getEdge_ :: Eidx -> g -> Maybe Edge
  freeEdgeIndicesOf_ :: Int -> g -> [Eidx]
  freeNodeIndicesOf_ :: Int -> g -> [Nidx]
  allNodes_ :: g -> [Node]
  allEdges_ :: g -> [Edge]
  work_ :: [Operation] -> g -> Maybe g

{-
sunP1 :: (FlatGraph g) => Edge -> g -> Maybe [VectorSpace Poly g]
sunP1 emn g
  | (Edge _ [nm, nn] G) <- emn
  , (Node _ nmEdges) <- nm
  , (nim, nkm) <- otherEdges nm emn
  , (njn, nln) <- otherEdges nn emn
  = do
      ni <- otherNode eim nm
-}

{- TODO add gluon check
tadPole :: (FlatGraph g) => Node -> g -> Maybe [VectorSpace Poly g]
tadPole ni g
  | (Node _ [eij, eik]) <- ni
  , (Edge _ _ eijType) <- eij
  , (Edge _ _ eikType) <- eik
  , eijType == eikType && eijType == U || eijType == D
  , eij == eik = Just [VS zero g]
  | otherwise = Nothing
-}


gluonExpansion :: (FlatGraph g) => Node -> g -> Maybe [VectorSpace Poly g]
gluonExpansion nl g =do
  lhs <- gluonExpansionHalf "clock" nl g
  rhs <- gluonExpansionHalf "anticlock" nl g
  return [VS plusOne lhs, VS minusOne rhs]

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
killChain :: (FlatGraph g) => Node -> g -> Maybe [VectorSpace Poly g]
killChain nj g
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
      return [VS plusOne g']
  | otherwise = Nothing

----

otherNode :: Edge -> Node -> Maybe Node
otherNode edge n
  | (Edge _ [n1, n2] _) <- edge
  , n == n1 && n /= n2 ||
    n == n2 && n /= n1
    = Just $ head [ ni | ni <- [n1, n2], ni /= n ]
  | otherwise = Nothing

instance Eq Node where
  (==) (Node a _) (Node b _) = a == b

instance Eq Edge where
  (==) (Edge a _ _) (Edge b _ _) = a == b

instance Show Node where
  show (Node nIDX edges) =
    (id "\nNode ") ++ show nIDX ++ id " "
    ++ show [ (eType, eIDX) | (Edge eIDX _ eType) <- edges ] ++ id " "

instance Show Edge where
  show (Edge eIDX nodes eType) =
    (id "\nEdge ") ++ show eIDX ++ id " "
    ++ show [ nIDX | (Node nIDX _) <- nodes ] ++ id " "
    ++ show eType
    ++ " "

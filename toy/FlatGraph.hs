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
data VectorSpace poly g = VS poly g deriving (Show)

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

-- FULL DECOMPOSITIONS --

sunP1 :: (FlatGraph g) => Edge -> g -> Maybe [VectorSpace Poly g]
sunP1 emn g
  | Just (lhs, rhs) <- sunP1Split emn g = Just [VS plusOne lhs, VS minusOverN rhs]
  | otherwise = Nothing

gluonExpansion :: (FlatGraph g) => Node -> g -> Maybe [VectorSpace Poly g]
gluonExpansion nl g =do
  lhs <- gluonExpansionHalf "clock" nl g
  rhs <- gluonExpansionHalf "anticlock" nl g
  return [VS plusOne lhs, VS minusOne rhs]

tadPole ni g
  | Just g' <- tadPoleSubGraph ni g = Just [VS zero g']
  | otherwise = Nothing

killChain :: (FlatGraph g) => Node -> g -> Maybe [VectorSpace Poly g]
killChain nj g
  | Just g' <- killChainSubGraph nj g = Just [VS plusOne g']
  | otherwise = Nothing

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

-- UTILS --

orientedDirectedTypes :: Node -> [EdgeType]
orientedDirectedTypes node = eTypes
  where
    (Node _ edges) = oriented node
    eTypes = filter (\e -> e/=G) $ edgeTypes edges

isAntiClock :: Node -> Bool
isAntiClock node
  | orientedDirectedTypes node == [U, D] = True
  | otherwise = False

isClock :: Node -> Bool
isClock node
  | orientedDirectedTypes node == [D, U] = True
  | otherwise = False

edgeTypes :: [Edge] -> [EdgeType]
edgeTypes edges = [ eType | Edge _ _ eType <- edges ]

edgeType :: Edge -> EdgeType
edgeType (Edge _ _ eType) = eType

invert :: Edge -> Edge
invert (Edge eIDX [n1, n2] eType) = Edge eIDX [n2, n1] (rotate eType)

orientEdge :: Node -> Edge -> Edge
orientEdge n0 edge
  | (Edge eIDX [n1, n2] eType) <- edge
  = if n0==n1 then edge else invert edge

oriented :: Node -> Node
oriented node
  | (Node nIDX edges) <- node
  = Node nIDX (map (orientEdge node) edges)

otherEdges :: Node -> Edge -> Maybe (Edge, Edge)
otherEdges (Node _ edges) edge
  | length nodeEdges == 2 = Just (nodeEdges !! 0, nodeEdges !! 1)
  | otherwise = Nothing
  where nodeEdges = [ e | e <- edges, e /= edge ]

otherNode :: Edge -> Node -> Maybe Node
otherNode edge n
  | (Edge _ [n1, n2] _) <- edge
  , n == n1 && n /= n2 ||
    n == n2 && n /= n1
    = Just $ head [ ni | ni <- [n1, n2], ni /= n ]
  | otherwise = Nothing

-- TYPE PROPERTIES --

class Rotatable e where
  rotate :: e -> e

instance Rotatable EdgeType where
  rotate U = D
  rotate D = U
  rotate G = G

instance Eq Node where
  (==) (Node a _) (Node b _) = a == b

instance Eq Edge where
  (==) (Edge a _ _) (Edge b _ _) = a == b

-- note that the nodes are oriented
instance Show Node where
  show node =
    (id "\nNode ") ++ show nIDX ++ id " "
    ++ show [ (eType, eIDX) | (Edge eIDX _ eType) <- edges ] ++ id " "
    where
      (Node nIDX edges) = oriented node

instance Show Edge where
  show (Edge eIDX nodes eType) =
    (id "\nEdge ") ++ show eIDX ++ id " "
    ++ show [ nIDX | (Node nIDX _) <- nodes ] ++ id " "
    ++ show eType
    ++ " "

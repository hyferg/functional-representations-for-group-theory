module FlatGraph where

data EdgeType = U | D | G deriving (Show, Eq)
type Nidx = Int
type Eidx = Int
data Node = Node Nidx [Edge]
data Edge = Edge Eidx [Node] EdgeType

data Operation = InsertE [Edge] | RemoveE [Edge] |
                 InsertN [Node] | RemoveN [Node] |
                 SubEdge [(Node, Edge, Edge)] |
                 Replace [(Edge, Edge)]

class FlatGraph g where
  getNode_ :: Nidx -> g -> Maybe Node
  getEdge_ :: Eidx -> g -> Maybe Edge
  freeEdgeIndicesOf_ :: Int -> g -> [Eidx]
  freeNodeIndicesOf_ :: Int -> g -> [Nidx]
  allNodes_ :: g -> [Node]
  allEdges_ :: g -> [Edge]
  work_ :: [Operation] -> g -> Maybe g

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

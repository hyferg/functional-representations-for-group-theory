module FlatGraph where

data EdgeType = U | D | G deriving (Show, Eq)
type Label = Int
data Node = Node Label [Edge]
data Edge = Edge Label [Node] EdgeType

data Operation = InsertE [Edge] | InsertN [Node] |
                 RemoveE [Edge] | Swap [(Node, Node)] |
                 Merge [(Node, Node)]

class FlatGraph g where
  getNode_ :: Label -> g -> Maybe Node
  getEdge_ :: Label -> g -> Maybe Edge
  freeEdgeLabelsOf_ :: Int -> g -> [Label]
  freeNodeLabelsOf_ :: Int -> g -> [Label]
  allNodes_ :: g -> [Node]
  allEdges_ :: g -> [Edge]
  split_ :: Node -> g -> Maybe ([Node], g)
  safeSplit_ :: Node -> g -> Maybe ([Node], g)
  work_ :: [Operation] -> g -> Maybe g

-- UTILS --

orientedDirectedTypes :: Node -> [EdgeType]
orientedDirectedTypes node = eTypes
  where
    (Node _ edges) = oriented node
    eTypes = filter (\e -> e/=G) $ edgeTypes edges

chiralEq :: Node -> Node -> Bool
chiralEq n1 n2 = isClock n1 && isClock n2 ||
  isAntiClock n1 && isAntiClock n2

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
invert (Edge label [n1, n2] eType) = Edge label [n2, n1] (rotate eType)

orientEdge :: Node -> Edge -> Edge
orientEdge n0 edge
  | (Edge _ [n1, _] _) <- edge
  = if n0==n1 then edge else invert edge

oriented :: Node -> Node
oriented node
  | (Node label edges) <- node
  = Node label (map (orientEdge node) edges)

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
    (id "\nNode ") ++ show nL ++ id " "
    ++ show [ (eType, eL) | (Edge eL _ eType) <- edges ] ++ id " "
    where
      (Node nL edges) = oriented node

instance Show Edge where
  show (Edge eL nodes eType) =
    (id "\nEdge ") ++ show eL ++ id " "
    ++ show [ nL | (Node nL _) <- nodes ] ++ id " "
    ++ show eType
    ++ " "

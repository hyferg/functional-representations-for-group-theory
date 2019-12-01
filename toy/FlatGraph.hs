module FlatGraph where

data EdgeType = R | B | G deriving (Show)
type Nidx = Int
type Eidx = Int
data Node = Node Nidx [Edge]
data Edge = Edge Eidx [Node] EdgeType

class FlatGraph g where
  getNode_ :: Nidx -> g -> Maybe Node
  getEdge_ :: Eidx -> g -> Maybe Edge
  freeEdgeIndicesOf_ :: Int -> g -> [Eidx]
  freeNodeIndicesOf_ :: Int -> g -> [Nidx]
  insertNodes_ :: [Node] -> g -> Maybe g
  insertEdges_ :: [Edge] -> g -> Maybe g
  allNodes_ :: g -> [Node]
  allEdges_ :: g -> [Edge]


otherNode :: Edge -> Node -> Node
otherNode (Edge _ [n1, n2] _) n
  | n == n1 && n /= n2 ||
    n == n2 && n /= n1
    = head [ ni | ni <- [n1, n2], ni /= n ]

killChain :: (FlatGraph g) => Node -> g -> Maybe g
killChain node g
  | (Node _ [e1, e2]) <- node
  = let
      n1 = otherNode e1 node
      n2 = otherNode e2 node
      e3 = Edge (head $ 1 `freeEdgeIndicesOf_` g) [n1, n2] G
    in return g >>= insertEdges_ [e3]
  | otherwise = Nothing

----

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

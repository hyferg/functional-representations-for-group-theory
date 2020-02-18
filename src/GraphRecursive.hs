module GraphRecursive (
  GraphRecursive(..),
  EdgeType(..), Label, Node(..), Edge(..),
  Operation(..),
  orientEdge, edgeType, edgeTypes, chiralEq, antiChiralEq,
  oriented, otherNode, rotate, is,
  LabelEquatable(..), ColorEquatable(..), Invertable(..) ) where

data EdgeType = U | D | G deriving (Show, Eq)
type Label = Int
data Node = N Label [Edge]
data Edge = E Label [Node] EdgeType

data Operation = InsertE [Edge] | InsertN [Node] |
                 RemoveE [Edge] | Swap [(Node, Node)] |
                 UpdateEdgeType [(Edge, EdgeType)] |
                 Merge [(Node, Node)] | DeleteE [Edge] | DeleteN [Node]

class GraphRecursive g where
  getNode :: Label -> g -> Maybe Node
  getEdge :: Label -> g -> Maybe Edge
  -- TODO no need for this to take an Int, just return an infinite list
  freeEdgeLabelsOf :: Int -> g -> [Label]
  freeNodeLabelsOf :: Int -> g -> [Label]
  allNodes :: g -> [Node]
  allEdges :: g -> [Edge]
  safeSplit :: Node -> g -> Maybe ([Node], g)
  work :: [Operation] -> g -> Maybe g
  swapChain :: (Node, Edge, Node, Edge, Node) -> Edge -> g -> Maybe g
  isEmpty :: g -> Bool

-- EXPORTS

orientEdge :: Node -> Edge -> Edge
orientEdge n0 edge
  | (E _ [n1, _] _) <- edge
  , n0 =@ n1 = edge
  | (E _ [n1, n2] _) <- edge
  , n0 =@ n2 || n0 /=@ n1 = rotate edge

oriented :: Node -> Node
oriented node
  | (N label edges) <- node
  = N label (map (orientEdge node) edges)

chiralEq :: Node -> Node -> Bool
chiralEq n1 n2
  | (N _ n1Edges) <- oriented n1
  , (N _ n2Edges) <- oriented n2
  , len <- length n1Edges
  = (edgeTypes n1Edges) `elem` [ take len $ drop i $ cycle (edgeTypes n2Edges) |
                             i <- [1..len] ]

antiChiralEq :: Node -> Node -> Bool
antiChiralEq n1 n2
  | (N _ n1Edges) <- oriented n1
  , (N _ n2Edges) <- oriented n2
  , len <- length n1Edges
  = (reverse $ edgeTypes n1Edges) `elem` [
      take len $ drop i $ cycle (edgeTypes n2Edges) | i <- [1..len] ]

edgeType :: Edge -> EdgeType
edgeType (E _ _ eType) = eType

otherNode :: Edge -> Node -> Maybe Node
otherNode edge n
  | (E _ [n1, n2] _) <- edge
  , n =@ n1 && n /=@ n2 ||
    n =@ n2 && n /=@ n1
    = Just $ head [ ni | ni <- [n1, n2], ni /=@ n ]
  | otherwise = Nothing

rotate :: Edge -> Edge
rotate (E label [n1, n2] eType) = E label [n2, n1] (invert eType)

is :: Edge -> EdgeType -> Bool
is (E _ _ a) b = a == b

-- UTILS --

edgeTypes :: [Edge] -> [EdgeType]
edgeTypes edges = [ eType | E _ _ eType <- edges ]


-- TYPE PROPERTIES --

class Invertable e where
  invert :: e -> e

instance Invertable EdgeType where
  invert U = D
  invert D = U
  invert G = G

class LabelEquatable o where
  (=@), (/=@) :: o -> o -> Bool
  x  =@ y = not (x /=@ y)
  x /=@ y = not (x  =@ y)

class ColorEquatable o where
  (=~), (/=~) :: o -> o -> Bool
  x /=~ y = not (x  =~ y)
  x  =~ y = not (x /=~ y)

instance LabelEquatable Node where
  (=@) (N a _) (N b _) = a == b

instance LabelEquatable Edge where
  (=@) (E a _ _) (E b _ _) = a == b

instance ColorEquatable Edge where
  (=~) (E _ _ a) (E _ _ b) = a == b

-- note that the nodes are oriented
instance Show Node where
  show node =
    (id "Node ") ++ show nL ++ id " "
    ++ show [ e | e <- edges ] ++ id "\n"
    where
      (N nL edges) = oriented node

instance Show Edge where
  show (E eL nodes eType) =
    (id "Edge ") ++ show eL ++ id " "
    ++ show [ nL | (N nL _) <- nodes ] ++ id " "
    ++ show eType
    ++ ""

module GraphRecursive (
  GraphRecursive(..),
  EdgeType(..), Label, Node(..), Edge(..),
  orientEdge, edgeType, edgeTypes, chiralEq, antiChiralEq,
  oriented, otherNode, rotate, isEdgeType, oneEdge, hasGhost,
  LabelEquatable(..), ColorEquatable(..),
  Invertable(..),
  ChiralEquatable(..)
  ) where

data EdgeType = U | D | G deriving (Show, Eq)
type Label = Int
data Node = N Label [Edge]
data Edge = E Label [Node] EdgeType | Ghost

class GraphRecursive g where
  updateEdgeType :: (Edge, EdgeType) -> g -> Maybe (Edge, g)
  splitNode :: Node -> g -> Maybe ([Node], g)
  splitNodeCenterOn :: Node -> Edge -> g -> Maybe ((Node, Node, Node), g)
  mergeNodes :: [Node] -> g -> Maybe (Node, g)
  product :: ([Node], [Edge]) -> g -> Maybe g
  removeNode :: Node -> g -> Maybe (Edge, g)
  removeEdge :: Edge -> g -> Maybe ((Node, Node), g)

  freeEdgeLabelsOf :: Int -> g -> [Label]
  freeNodeLabelsOf :: Int -> g -> [Label]
  getNode :: Label -> g -> Maybe Node
  getEdge :: Label -> g -> Maybe Edge
  allNodes :: g -> [Node]
  allEdges :: g -> [Edge]
  isEmpty :: g -> Bool

-- EXPORTS

isGhost :: Edge -> Bool
isGhost (Ghost) = True
isGhost _ = False

hasGhost :: Node -> Bool
hasGhost (N _ edges)
  | 1 <= (length $ filter (isGhost) edges)
  = True
  | otherwise = False

orientEdge :: Node -> Edge -> Edge
orientEdge n0 edge
  | (E _ [n1, _] _) <- edge
  , n0 =@ n1 = edge
  | (E _ [n1, n2] _) <- edge
  , n0 =@ n2 || n0 /=@ n1 = rotate edge
  | Ghost <- edge = Ghost

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

isEdgeType :: Edge -> EdgeType -> Bool
isEdgeType (E _ _ a) b = a == b

edgeTypes :: [Edge] -> [EdgeType]
edgeTypes edges = [ eType | E _ _ eType <- edges ]

oneEdge :: Node -> Maybe Edge
oneEdge (N _ [e]) = Just e
oneEdge (N _ _) = Nothing

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

class ChiralEquatable o where
  -- chiral eq
  (=*), (/=*) :: o -> o -> Bool
  (=**), (/=**) :: o -> o -> Bool
  x /=* y = not (x  =* y)
  x  =* y = not (x /=* y)

  -- anti chiral eq
  x /=** y = not (x  =** y)
  x  =** y = not (x /=** y)

instance ChiralEquatable Node where
  (=*) n1 n2 = chiralEq n1 n2
  (=**) n1 n2 = antiChiralEq n1 n2

instance LabelEquatable Node where
  (=@) (N a _) (N b _) = a == b

instance LabelEquatable Edge where
  (=@) (E a _ _) (E b _ _) = a == b

instance ColorEquatable Edge where
  (=~) (E _ _ a) (E _ _ b) = a == b
  (=~) (Ghost) (Ghost) = True
  (=~) _ _ = False

-- note that the nodes are oriented
instance Show Node where
  show node =
    (id "Node ") ++ show nL ++ id " "
    ++ show [ e | e <- edges ]
    where
      (N nL edges) = oriented node

instance Show Edge where
  show (E eL nodes eType) =
    (id "Edge ") ++ show eL ++ id " "
    ++ show [ nL | (N nL _) <- nodes ] ++ id " "
    ++ show eType
    ++ ""
  show (Ghost) = id "Ghost"

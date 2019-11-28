module EdgeNode where

data NodeType = GG | GGG | Clock | AntiClock | Unidentified deriving (Show, Eq)
data Node = Node NodeType [Int] deriving (Show)
data EdgeType = Up | Down | Gluon | None deriving (Show, Eq)
data Edge = Edge EdgeType (Int, Int) deriving (Show)

class Invertable a where
  invert :: a -> a

instance Invertable EdgeType where
  invert Up = Down
  invert Down = Up
  invert other = other

filterJustOneEdge :: EdgeType -> [(i, Edge)] -> Maybe (i, Edge)
filterJustOneEdge targetEdgeType edges
  | length edgeMatch == 1 = Just $ head edgeMatch
  | otherwise = Nothing
  where
    edgeMatch = [ (i, Edge edgeType (a,b)) |
                  (i, Edge edgeType (a,b)) <- edges,
                  targetEdgeType == edgeType ]



flipEdge :: Edge -> Edge
flipEdge (Edge edgeType (a,b)) = (Edge (invert edgeType) (b,a))

updateNodeType :: NodeType -> Node -> Node
updateNodeType nodeType (Node _ b) = Node nodeType b

replaceEdgeIDXInNode :: Int -> Int -> Node -> Node
replaceEdgeIDXInNode targetEdgeIDX newEdgeIDX (Node _ edgeIDXs) = (Node Unidentified newEdgeIDXs)
  where
    newEdgeIDXs = [ if i == targetEdgeIDX then newEdgeIDX else i | i <- edgeIDXs]


removeEdgeIDXfromNode :: Int -> Node -> Node
removeEdgeIDXfromNode edgeIDX (Node _ edgeIDXs) = (Node Unidentified newEdgeIDXs)
  where
    newEdgeIDXs = [ i | i <- edgeIDXs, i /= edgeIDX]

appendEdgeIDXtoNode :: Int -> Node -> Node
appendEdgeIDXtoNode edgeIDX (Node a edgeIDXs) = Node a (edgeIDXs ++ [edgeIDX])

isGGG :: [EdgeType] -> Bool
isGGG edgeTypes
  | numEdges == 3 && numGluonEdges == 3 = True
  | otherwise = False
  where
    numEdges = length edgeTypes
    numGluonEdges = length [ True | e <- edgeTypes, e == Gluon ]

directedEdges :: [EdgeType] -> [EdgeType]
directedEdges edgeTypes = [ e | e <- edgeTypes,
                            e == Up || e == Down ]

isClockLike :: [EdgeType] -> Bool
isClockLike edgeTypes
  | numEdges == 3 && numClockEdges == 2 = True
  | otherwise = False
  where
    numEdges = length edgeTypes
    clockEdges = directedEdges edgeTypes
    numClockEdges = length clockEdges

isClock :: [EdgeType] -> Bool
isClock edgeTypes = isClockLike edgeTypes &&
  [Down, Up] == directedEdges edgeTypes

isAntiClock :: [EdgeType] -> Bool
isAntiClock edgeTypes = isClockLike edgeTypes &&
  [Up, Down] == directedEdges edgeTypes


isGGChain :: [EdgeType] -> Bool
isGGChain edgeTypes = edgeTypes == [Gluon, Gluon]

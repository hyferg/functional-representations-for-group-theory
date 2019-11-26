module EdgeNode where

data NodeType = GGG | Clock | AntiClock | Unidentified deriving (Show, Eq)
data Node = Node NodeType [Int] deriving (Show)
data EdgeType = Up | Down | Gluon | None deriving (Show, Eq)
data Edge = Edge EdgeType (Int, Int) deriving (Show)

class Invertable a where
  invert :: a -> a

instance Invertable EdgeType where
  invert Up = Down
  invert Down = Up
  invert other = other

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
  -- TODO could use a better check to not accept [Down, Down, Gluon]
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

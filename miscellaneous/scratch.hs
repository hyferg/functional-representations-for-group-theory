module EdgeGraph where
data EdgeType = Up | Down | Gluon deriving (Show, Eq)
data Edge = Edge EdgeType (Int, Int) deriving (Show)
data Edges = Edges [Edge] deriving (Show)

class Graph g where
  remove :: subgraph -> g -> g

class Invertable a where
  invert :: a -> a

instance Invertable EdgeType where
  invert Up = Down
  invert Down = Up
  invert other = other


fileContent =  fmap lines (readFile "./gridEdges.txt")
x = fmap (\x -> read x :: (Int, Int)) <$> fileContent

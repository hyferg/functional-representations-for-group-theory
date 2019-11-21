module EdgeSpaces where
data NodeType = I | E deriving(Show, Eq)
data Node = Node NodeType Int deriving(Show)
data EdgeType = Up | Down | Gluon deriving(Show, Eq)
data Edge = Edge EdgeType (Node, Node) deriving(Show)
data EdgeSpace poly = EdgeSpace poly [Edge] deriving(Show)
data EdgeSpaces poly = EdgeSpaces [(EdgeSpace poly)] deriving(Show)

anyInternalTensorLinks :: [Edge] -> [(Int, Int)]
anyInternalTensorLinks edges = [
  (i, j) |
    Edge _ (Node iNodeType i, Node jNodeType j) <- edges,
    iNodeType == I || jNodeType == I
    ]

tuplesToList :: [(a,a)] -> [a]
tuplesToList ((a,b):xs) = a : b : tuplesToList xs
tuplesToList _ = []

anyIndices :: [Edge] -> [Int]
anyIndices edges = indices
  where
    links = [ (i, j) | Edge _ (Node _ i, Node _ j) <- edges ]
    indices = [ i | i <- tuplesToList links ]

internalIndices :: [Edge] -> [Int]
internalIndices edges = indices
  where
  links = anyInternalTensorLinks edges
  indices = [ i | i <- tuplesToList links, i < 0 ]

allInternalEdges :: [Edge] -> [Edge]
allInternalEdges edges = [
  Edge edgeType (Node iNodeType i, Node jNodeType j) |
  Edge edgeType (Node iNodeType i, Node jNodeType j) <- edges,
  iNodeType == I || jNodeType == I ]

externalTensorLinks :: (EdgeSpace a) -> [(Int, Int)]
externalTensorLinks (EdgeSpace _ edges) = [
  (i, j) |
    Edge _ (Node iNodeType i, Node jNodeType j) <- edges,
    iNodeType == E || jNodeType == E
    ]

internalExternalTensorLinks :: (EdgeSpace a) -> [(Int, Int)]
internalExternalTensorLinks (EdgeSpace _ edges) = [
  (i, j) |
    Edge _ (Node iNodeType i, Node jNodeType j) <- edges,
    iNodeType == E && jNodeType == I || iNodeType == I && jNodeType == E
    ]


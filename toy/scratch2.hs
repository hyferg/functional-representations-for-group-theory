data NodeType = GG | GGG | Clock | AntiClock | Other deriving(Show, Eq)
data EdgeType = Up | Down | Gluon deriving(Show)
data Edge = Edge EdgeType [Node] Graph
data Node = Node [Edge] Graph
data Graph = Graph [Edge] [Node]

graph = let
  n0 = Node [e1] g
  e1 = Edge Gluon [n0, n1] g
  n1 = Node [e1] g
  g = Graph [e1] [n0, n1]
  in g

nn = f $ head $ nodes graph
gg = graphFromNode nn

nodes :: Graph -> [Node]
nodes (Graph _ nodes) = nodes

edges :: Graph -> [Edge]
edges (Graph edges _) = edges

graphFromEdge :: Edge -> Graph
graphFromEdge (Edge _ _ g) = g

graphFromNode :: Node -> Graph
graphFromNode (Node _ g) = g

f :: Node -> Node
f node
  | Node [Edge Gluon ns g] _ <- node
  , Graph edges nodes <- g
  = let
  newGraph = Graph (edges ++ [newEdge]) (nodes)

  newEdge = Edge Up [] newGraph

  subNode = Node [Edge Gluon ns newGraph, newEdge] newGraph

  in subNode

--  | otherwise = node

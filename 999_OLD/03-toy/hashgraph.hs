data NodeType = GG | GGG | Clock | AntiClock | Other deriving(Show, Eq)
data EdgeType = Up | Down | Gluon deriving(Show)
data Edge = Edge EdgeType [Node]
data Node = Node [Edge]
data Graph = Graph [Edge] [Node]

(edges, nodes) = let
  n0 = Node[e1, e2]
  e1 = Edge Gluon [n0, n1]
  e2 = Edge Gluon [n0, n1]
  n1 = Node [e1, e2]
  in ([e1, e2], [n0, n1])

changeEdgeType :: EdgeType -> Edge -> Edge
changeEdgeType edgeType (Edge _ b) = Edge edgeType b

nodeType :: Node -> NodeType
nodeType (Node [Edge Gluon _, Edge Gluon _]) = GG
nodeType (Node [Edge Gluon _, Edge Gluon _, Edge Gluon _]) = GGG
nodeType _ = Other

numEdges (Node edges) = length edges

edgeType (Edge edgeType_ _) = edgeType_

edgeTypes (Node edges) = [ edgeType e | e <- edges]

adjacentNodes (Edge _ nodes) = nodes

neighborNodes (Node edges) = [ n | e <- edges, n <- adjacentNodes e]

gluonExpansion (Node [Edge Gluon n1s,Edge Gluon n2s,Edge Gluon n3s])

addFunc (Node [Edge Gluon lns, Edge Gluon rns]) = let
  dangleNode = Node [lonelyEdge]
  lonelyEdge = Edge Down [dangleNode, ns]
  ns = (Node [Edge Up lns, Edge Up rns, lonelyEdge])
  in ns

func (Node [Edge Gluon n1s, Edge Gluon n2s]) = Node [Edge Gluon n1s]

removeEdge 


g = Graph edges nodes
nNew = func $ nodes !! 0

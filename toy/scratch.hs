data NodeType = GG | GGG | Clock | AntiClock | Other deriving(Show, Eq)
data EdgeType = Up | Down | Gluon deriving(Show)
data Edge = Edge EdgeType [Node] String
data Node = Node [Edge] String
data Graph = Graph [Edge] [Node]

(edges, nodes) = let
  n0 = Node [e1,e2,e3] "asjfl"
  n1 = Node [e1] "0oisjckje"
  n2 = Node [e2] "sdk3nj,"
  n3 = Node [e3] "asldkfje"
  e1 = Edge Gluon [n1, n0] "asxi0j"
  e2 = Edge Gluon [n2, n0] "asdj0jx"
  e3 = Edge Gluon [n3, n0] "x,i3jnd"
  in ([e1, e2, e3], [n0, n1, n2, n3])

changeEdgeType :: EdgeType -> Edge -> Edge
changeEdgeType edgeType (Edge _ b c) = Edge edgeType b c

nodeType :: Node -> NodeType
nodeType (Node [Edge Gluon _ _, Edge Gluon _ _] _) = GG
nodeType (Node [Edge Gluon _ _, Edge Gluon _ _, Edge Gluon _ _] _) = GGG
nodeType _ = Other

numEdges (Node edges _) = length edges

edgeType (Edge edgeType_ _ _) = edgeType_

edgeTypes (Node edges _) = [ edgeType e | e <- edges]

adjacentNodes (Edge _ nodes _) = nodes

neighborNodes (Node edges _) = [ n | e <- edges, n <- adjacentNodes e]

{-
gluonExpansion (Node [Edge Gluon n1s, Edge Gluon n2s, Edge Gluon n3s]) = let
  n1 = Node [Edge Gluon n1s, e1, e3]
  n2 = Node [Edge Gluon n2s, e2, e1]
  n3 = Node [e3, e2, Edge Gluon n3s]
  e1 = Edge Up [n1, n2]
  e2 = Edge Up [n2, n3]
  e3 = Edge Up [n1, n3]
  in n1
-}

hashEq :: Node -> Node -> Bool
hashEq (Node _ n1hash) (Node _ n2hash) = n1hash ==n2hash

nodeNotNode :: Edge -> Node -> Node
nodeNotNode (Edge _ [n1, n2] _) nTarget = notNode [n1, n2] nTarget

notNode [n1, n2] nTarget = head [ n | n <- [n1, n2], not $ hashEq n nTarget ]


gluonExpansionV2 ( Node [ ei, ej, ek ] nCenterHash )
  | Edge Gluon nis ein <- ei
  , Edge Gluon njs ejn <- ej
  , Edge Gluon nks ekn <- ek
  , ni <- notNode nis tripleGluonNode
  , nj <- notNode njs tripleGluonNode
  , nk <- notNode nks tripleGluonNode = let

      ei = Edge Gluon [ni, n1] ein
      ej = Edge Gluon [nj, n2] ejn
      ek = Edge Gluon [nk, n3] ekn

      e1 = Edge Down [n1, n2] "asdfjie"
      e2 = Edge Down [n2, n3] "3jlx9"
      e3 = Edge Down [n3, n1] "x1nl"

      n1 = Node [ei, e1, e3] "aksldjfl"
      n2 = Node [e1, ej, e2] "xj1o"
      n3 = Node [e2, ek, e3] "jx90j1"
      in ei
  where
    tripleGluonNode = Node [ ei, ej, ek ] nCenterHash

--gluonExpansion :: Node -> Bool
gluonExpansion (Node [ Edge Gluon [i, Node _ iin] ein,
                       Edge Gluon [j, Node _ jjn] ejn,
                       Edge Gluon [k, Node _ kkn] ekn ] _)
  | iin == jjn && jjn == kkn = let
      ei = Edge Gluon [i, n1] ein
      ej = Edge Gluon [j, n2] ejn
      ek = Edge Gluon [k, n3] ekn

      e1 = Edge Down [n1, n2] "asdfjie"
      e2 = Edge Down [n2, n3] "3jlx9"
      e3 = Edge Down [n3, n1] "x1nl"

      n1 = Node [ei, e1, e3] "aksldjfl"
      n2 = Node [e1, ej, e2] "xj1o"
      n3 = Node [e2, ek, e3] "jx90j1"
      in ei

  {-
addFunc (Node [Edge Gluon lns, Edge Gluon rns]) = let
  dangleNode = Node [lonelyEdge]
  lonelyEdge = Edge Down [dangleNode, ns]
  ns = (Node [Edge Up lns, Edge Up rns, lonelyEdge])
  in ns

-}


g = Graph edges nodes
gggNode = nodes !! 0

module EdgeSpaces where
data Node = I Char | E Int
data Edge = Up (Node, Node) | Down (Node, Node) | Gluon (Node, Node)
data EdgeSpace poly = EdgeSpace poly [Edge]
data EdgeSpaces poly = EdgeSpaces [(EdgeSpace poly)]

import Graph
import FlatGraph
import Decompositions
import AddGraph
import MathObj.LaurentPolynomial as LP

g = addSunP1 emptyGraph
x = (allNodes <$> g, allEdges_ <$> g)

  {-
out = do
  sp1 <- addSunP1 emptyGraph
  edges <- Just $ allEdges_ sp1
  gluonEdges <- Just $ filter (\(Edge _ _ eType) -> eType==G) edges
  return gluonEdges
  sunP1 (gluonEdges !! 0) sp1
-}

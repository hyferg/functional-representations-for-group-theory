import Graph
import FlatGraph
import AddGraph
import MathObj.LaurentPolynomial as LP

g = addRing emptyGraph
x = (allNodes <$> g, allEdges_ <$> g)

{-
xx = do
  g <- addRing emptyGraph
  ns <- Just $ allNodes_ g
  g' <- (return g >>= killChain (ns !! 1))
  return (allNodes_ g', allEdges_ g')
-}

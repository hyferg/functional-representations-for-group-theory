import Graph
import FlatGraph
import Decompositions
import AddGraph
import MathObj.LaurentPolynomial as LP

-- g = handBag emptyGraph
-- x = (allNodes <$> g, allEdges_ <$> g)

{-
(ni) __eij__ (nj) __ejk__ (nk)

(ni) __eij__ (nj) __ejk__ (nk)
-}

out = do
  g <- pill emptyGraph
  eg <- Just $ (allEdges_ g) !! 1
  sunP1LHS eg g

{-
out = do
  g <- tripleGluon emptyGraph
  nc <- Just $ (allNodes_ g) !! 3
  g' <- gluonExpansionHalf "anticlock" nc g
  return g'
-}

{-
-}

nodes = allNodes_ <$> out
edges = allEdges_ <$> out

import Graph
import FlatGraph
import Decompositions
import AddGraph
import Debug
import MathObj.LaurentPolynomial as LP

g = handBag emptyGraph
x = (allNodes <$> g, allEdges_ <$> g)


out = do
  gh <- addSunP1 emptyGraph
  edges <- Just $ allEdges_ gh
  sunP1SplitDebug (edges !! 0) gh
  --return $ formsSunP1 (edges !! 0) gh


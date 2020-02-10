import Graph
import FlatGraph
import Decompositions
import AddGraph
import MathObj.LaurentPolynomial as LP
import Data.Maybe
import Data.Tree

main :: IO ()
main = return ()

-- poly' == poly as a rule in general
-- however one could imagine taking the input poly coeff and distributing it
-- over the child nodes so there could potentially be an exception
{-

-}

build :: (Graph -> Maybe Graph) -> Graph
build someGraphOn
  | Just g <- someGraphOn emptyGraph = g
  | otherwise = emptyGraph

input = build vector

n0 = (allNodes_ input) !! 0
n1 = (allNodes_ input) !! 1
e =  (allEdges_ input) !! 0

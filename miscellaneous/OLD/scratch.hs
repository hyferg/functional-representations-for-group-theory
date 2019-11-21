import Tensor
import EdgeSpaces
import Text.Pretty.Simple (pPrint)
import Data.Map.Strict as Map
import qualified MathObj.LaurentPolynomial as LP



peaceEdges = [
  Edge Gluon (Node I (1), Node I (2)),
  Edge Gluon (Node I (1), Node I (4)),
  Edge Gluon (Node I (1), Node I (3)),
  Edge Gluon (Node I (2), Node I (3)),
  Edge Gluon (Node I (2), Node I (4)),
  Edge Gluon (Node I (3), Node I (4))
  ]


edgePattern = [
  Edge Gluon (Node I (1), Node E (-1)),
  Edge Gluon (Node I (1), Node E (-2)),
  Edge Gluon (Node I (1), Node E (-3))
  ]

substitution = [
  Edge Gluon (Node I (1), Node E (-1)),
  Edge Gluon (Node I (1), Node E (-2)),
  Edge Gluon (Node I (1), Node E (-3))
  ]


emptyPeaceMapIntTensor = constructEmpty peaceEdges 1


y = construct substitution 5

out = do
  (links, peaceMapIntTensor) <- addEdges peaceEdges ([], emptyPeaceMapIntTensor)
  out <- matchPatternWithMapIndex edgePattern peaceMapIntTensor
  subgraphEmpty <- Just $ constructEmpty substitution (size peaceMapIntTensor + 1)
  subgraph <- Just $ addEdges substitution ([], subgraphEmpty)
  Just $ subgraphEmpty




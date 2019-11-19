import Tensor
import MathObj.LaurentPolynomial as LP
import EdgeSpaces
import Text.Pretty.Simple (pPrint)
import Data.Map.Strict as Map


edges = [ Edge Down  (Node I (-1), Node E 1),
            Edge Gluon (Node I (-1), Node I (-2)),
            Edge Up    (Node I (-1), Node E 2),
            Edge Up    (Node I (-2), Node E 3),
            Edge Down  (Node I (-2), Node E 4) ]

sunP1 :: EdgeSpace (LP.T Int)
sunP1 = EdgeSpace (fromCoeffs [1]) edges

vs = vectorSpaceFromEdgeSpace sunP1
tensors (TensorProduct poly tensors) = tensors

--internalTensors :: EdgeSpace -> VectorSpace





import SunP1
import Tensor
import Data.Map.Strict as Map
import MathObj.LaurentPolynomial as LP
import Data.Maybe
import Data.Tree as Tree

main :: IO ()
main = return ()

t0 = Tensor $ Map.fromAscList $ zip [0,1..] [
   Pointer (0,1) Down ,
   Pointer (0,0) Up ,
   Pointer (1,0) Gluon
  ]

t1 = Tensor $ Map.fromAscList $ zip [0,1..] [
   Pointer (0,2) Gluon ,
   Pointer (1,2) Down ,
   Pointer (1,1) Up
  ]

t2 = Tensor $ Map.fromAscList $ zip [0,1..] [
   Pointer (0,2) Gluon ,
   Pointer (1,2) Up ,
   Pointer (1,1) Down
  ]

identity = fromCoeffs [1]
dumbellTensors = Tensors $ Map.fromAscList $ zip [0,1..] [t0, t1]
dumbell = TensorProduct identity dumbellTensors

  {-
maybeTensors = fst $ decomposeP1 dumbell
maybeLinks = snd $ decomposeP1 dumbell
z (Just list) = list
final = rewireSingle (head $ Data.Maybe.fromJust maybeLinks) maybeTensors
-}

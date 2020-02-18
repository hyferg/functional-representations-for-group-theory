import Algo
import Graphs
import Data.Tree
import MathObj.LaurentPolynomial as LP
--import Poly (equivalent)

main :: IO ()
main = return ()

--input = build peace6j
--out = unfoldTree buildNode input
--rep = putStr $ drawTree $ fmap show out

strat = sonStrat

poly x = foldTree foldNode $ unfoldTree (buildNode strat) $ build x
med = unfoldTree (buildNode strat) $ build peace6j
poly1 = poly peace6j

{-
ids = [
  fromShiftCoeffs (0) [-1,0,1], -- pill
  fromShiftCoeffs (0) [], -- one
  fromShiftCoeffs (0) [-1,0,1], -- two
  fromShiftCoeffs (-1) [2,0,-3,0,1], -- three
  fromShiftCoeffs (0) [], -- four TODO
  fromShiftCoeffs (-1) [-4,0,5,0,-1] -- peace6j
  ]
-}

out = map poly [pill, oneCasimir, twoCasimir,
                threeCasimir, fourCasimir, peace6j]

--check = map (\(a,b) -> identical a b) $ zip ids out



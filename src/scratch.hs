{-
-- https://hackage.haskell.org/package/numeric-prelude
import MathObj.Polynomial as Poly
import MathObj.Polynomial.Core as Core
https://hackage.haskell.org/package/besout-0.2.0.1/docs/Bezout.html
https://hackage.haskell.org/package/poly-0.3.3.0/docs/Data-Poly.html
-}
import Data.Poly as Poly
import Data.Ratio as Ratio
import Data.Euclidean as Eu

x2 = X^2 :: VPoly (Ratio Integer)
x = X^1 :: VPoly (Ratio Integer)

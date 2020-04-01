{-
{-# LANGUAGE PatternSynonyms            #-}
module Poly (Poly, mul, add, zero, plusOne, plusN,
             minusOne, minusOverN) where

import Data.Poly (VPoly, pattern X)
import Data.Ratio as Ratio
import Data.Euclidean as Eu

type Poly = VPoly (Ratio Integer)

zero = 0 :: Poly

plusOne = 1 :: Poly

plusN = X :: Poly

minusOne = -1 :: Poly

minusOverN = (-1) * X^(-1) :: Poly

mul = (*)
add = (+)
-}

module Poly (Poly, mul, add, identical,
             zero, half, plusOne, plusN,
             minusOne, minusOverN) where

import MathObj.LaurentPolynomial
import Data.Ratio as Ratio

type Poly = T (Ratio Integer)

zero = fromCoeffs [0] :: Poly
half = fromCoeffs [1 % 2] :: Poly
plusOne = fromCoeffs [1] :: Poly
plusN = fromCoeffs [0,1] :: Poly
minusOne = fromCoeffs [-1] :: Poly
minusOverN = fromShiftCoeffs (-1) [-1] :: Poly

--TODO pretty print
--TODO equivalent for different reps


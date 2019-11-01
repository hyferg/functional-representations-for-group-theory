module Tensor  where
import Data.Map.Strict as Map
import MathObj.LaurentPolynomial as LP
data Newness a = Original a | New a deriving (Show)
data EdgeType = Up | Down | Gluon deriving (Show)
data Pointer = Pointer (Int, Int) EdgeType deriving (Show)
data Tensor = Tensor (Map Int Pointer) deriving (Show)
data Tensors = Tensors (Map Int Tensor) deriving (Show)
data VectorSpace = TensorProduct (LP.T Int) Tensors deriving (Show)

class EqEdgeType a where
  edgeEq :: a -> a -> Bool

  {-
instance EqEdgeType Pointer where
  edgeEq (Gluon _) (Gluon _) = True
  edgeEq (Up _) (Up _) = True
  edgeEq (Down _) (Down _) = True
  edgeEq _ _ = False


-}

isGluon :: Pointer -> Bool
isGluon (Pointer _ Gluon) = True
isGluon (_) = False

pointerVals (Pointer (i,j) _) = (i,j)


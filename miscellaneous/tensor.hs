import Data.List
import MathObj.LaurentPolynomial as LP

data IndexType = Up | Down | Gluon deriving (Show)

-- pointer (VectorSpace Object Index, VectorSpace Object Internal Index)
data Pointer = Pointer IndexType (Int, Int) deriving (Show)
data IndexableObject = Tensor [Pointer] deriving (Show)
data VectorSpace a = TensorProduct (LP.T a) [IndexableObject] deriving (Show)
data State a = Original a | New a deriving (Show)

pointers (Tensor t) = t
pointersOnlyIndices (Tensor t) indices = [
  pointer | (pointer, idx) <- zip t [0,1..], (idx `elem` indices)]
pointersExceptIndices (Tensor t) indices = [
  pointer | (pointer, idx) <- zip t [0,1..], not (idx `elem` indices)]

pointData (Pointer _ a) = a
pointsData t = map pointData t

{-[
  otherTensorSpaceIDX,
  [(selfIDX, otherTensorInternalIDX),(...)]
]
-}
selfInternalIDX x = fst x
otherTensorSpaceIDX x = fst $ snd x
otherTensorInternalIDX x = snd $ snd x
doublePointInfo (Tensor t) = [
  (
    (otherTensorSpaceIDX x),
    [
      (selfInternalIDX x, (otherTensorInternalIDX x)),
      (selfInternalIDX y, (otherTensorInternalIDX y))
    ]
  ) |
    (x:ys) <- tails $ zip [0,1..] (pointsData t),
    y <- ys,
    (otherTensorSpaceIDX x) == (otherTensorSpaceIDX y)]

popBubbleTensors :: [IndexableObject] -> State [IndexableObject]
popBubbleTensors tensors
  | [] == aDoublePoint = Original tensors
  where
    aDoublePoint = zip [0,1..] (head $ map doublePointInfo tensors)


-- bubbleInfo (TensorProduct poly tensors) = [
-- head $ doublePointInfo x | x <- tensors,
--   not (null $ doublePointInfo x)]

t1 = Tensor [Pointer Gluon (-1,0), Pointer Up (1,2), Pointer Down (1,1)]
t2 = Tensor [Pointer Gluon (-2,0), Pointer Up (0,2), Pointer Down (0,1)]
t3 = Tensor [Pointer Gluon (-1,0), Pointer Up (1,2), Pointer Down (2,1)]

vs = TensorProduct (fromCoeffs [1]) [t1, t2, t3]

import Data.List
import Data.Map.Strict as Map
import MathObj.LaurentPolynomial as LP
data Newness a = Original a | New a deriving (Show)
data IndexType = Up | Down | Gluon deriving (Show)
data Pointer = Pointer (Int, Int) IndexType deriving (Show)
data Tensor = Tensor (Map Int Pointer) deriving (Show)
data Tensors = Tensors (Map Int Tensor) deriving (Show)
data VectorSpace = TensorProduct (LP.T Int) Tensors deriving (Show)

class State s where
  (-:) :: s a -> (a -> a) -> s a
instance State Newness where
  (Original x) -: f = New (f x)
  (New x) -: f = New (x)


identity = fromCoeffs [1]

t1 = Tensor $ Map.fromAscList $ zip [0,1..] [
  Pointer (-1,0) Gluon,
  Pointer (1,1) Down,
  Pointer (1,0) Up
  ]

t2 = Tensor $ Map.fromAscList $ zip [0,1..] [
  Pointer (0,2) Down,
  Pointer (0,1) Up,
  Pointer (-2,0) Gluon
  ]

t3 = Tensor $ Map.fromAscList $ zip [0,1..] [
  Pointer (0,0) Down,
  Pointer (1,0) Up,
  Pointer (2,0) Gluon
  ]

tensors = Tensors $ Map.fromAscList $ zip [0,1..] [t1, t2]

vs = TensorProduct identity tensors

--nonBubblePointer :: Tensor -> Maybe Pointer
{-
nonBubblePointer (Tensor pointers)
  | nonBubbleIDXs /= [] = Just $ head nonBubbleIDXs
  | otherwise = Nothing
  where
  vsIDXs = [ vsIDX | (_, Pointer (vsIDX, _) _) <- toAscList pointers]
  uniqueIDX x = if 2 == length grouped then uniqueOf grouped else []
    where
      uniqueOf x = [ head i | i <- x, length i == 1]
      grouped = group $ sort x
  nonBubbleIDXs = uniqueIDX vsIDXs
-}
nonBubblePointer (Tensor pointers)
  | 0 < length uniqueElems = Just $ head uniqueElems
  | otherwise = Nothing
  where
    pointersList = [ pointer | (_, pointer) <- toAscList pointers]
    pointsSame0 (Pointer vals0 _) (Pointer vals1 _) = fst vals0 == fst vals1
    groupedPointers = groupBy pointsSame0 pointersList
    uniqueOf xg = [ head i | i <- xg, length i == 1]
    uniqueElems = if 2 == length groupedPointers then uniqueOf groupedPointers else []

--wire :: Tensor -> Tensor -> Maybe (Int,Int,Int,Int)


--popABubble :: VectorSpace -> State VectorSpace

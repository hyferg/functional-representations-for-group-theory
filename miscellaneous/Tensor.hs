module Tensor  where
import Data.Map.Strict as Map
import Data.List
import MathObj.LaurentPolynomial as LP
data Newness a = Original a | New a deriving (Show)
data EdgeType = Up | Down | Gluon deriving (Show, Eq)
data Pointer = Pointer (Int, Int) EdgeType deriving (Show, Eq)
data Tensor = Tensor (Map Int Pointer) deriving (Show, Eq)
data Tensors = Tensors (Map Int Tensor) deriving (Show)
data VectorSpace = TensorProduct (LP.T Int) Tensors deriving (Show)

class State s where
  (-:) :: s a -> (a -> a) -> s a

instance State Newness where
  (Original x) -: f = New (f x)
  (New x) -: _ = New (x)

class EqEdgeType a where
  edgeEq :: a -> a -> Bool

instance EqEdgeType Pointer where
  edgeEq (Pointer _ Gluon) (Pointer _ Gluon) = True
  edgeEq (Pointer _ Up) (Pointer _ Up) = True
  edgeEq (Pointer _ Down) (Pointer _ Down) = True
  edgeEq _ _ = False

isGluon :: Pointer -> Bool
isGluon (Pointer _ Gluon) = True
isGluon _ = False

keylessMapToList :: Map k a -> [a]
keylessMapToList m = [a | (_, a) <- Map.toAscList m]

pointers :: Tensor -> [Pointer]
pointers (Tensor pointersMap) = keylessMapToList pointersMap

tensorsList :: Tensors -> [Tensor]
tensorsList (Tensors tensorsMap) = keylessMapToList tensorsMap

nonGluonPointers :: Tensor -> [Pointer]
nonGluonPointers t = Prelude.filter (not . isGluon) $ pointers t

mappedKeys :: Tensor -> [(Int, Int)]
mappedKeys t = [ (i, j) | (Pointer (i,j) _) <- pointers t]

uniqueVsKeys :: Tensor -> [Int]
uniqueVsKeys t = nub [ i | (i, _)<- mappedKeys t]

tensorPairs :: Tensors -> [(Tensor, Tensor)]
tensorPairs (Tensors tensorsMap) =
  [ (x,y) | (x:ys) <- tails $ keylessMapToList tensorsMap, y <- ys ]

tensorMapPairs :: Tensors -> [((Int, Tensor), (Int, Tensor))]
tensorMapPairs (Tensors tensorsMap) =
  [ (x,y) | (x:ys) <- tails $ Map.toAscList tensorsMap, y <- ys ]

{-
The same ordering of Up/Down pointers in two gluon connected tensors
indicates that it a P1 operator. Maybe needs a test to make sure the
tensor is not (Gluon Down Down) etc.
-}
sunP1 :: Tensor -> Tensor -> Bool
sunP1 t1 t2 = 2 == length [ True | (i, j) <- zip x y, edgeEq i j]
  where
    x = nonGluonPointers t1
    y = nonGluonPointers t2

sunP1Link :: Tensors -> Maybe ((Int, Tensor), (Int, Tensor))
sunP1Link tensors
  | [] == sunP1Tensors_ = Nothing
  | otherwise = Just $ head sunP1Tensors_
  where
    sunP1Tensors_ =
      [((k1, t1), (k2, t2)) | ((k1, t1), (k2, t2)) <- tensorMapPairs tensors, sunP1 t1 t2]

p1UpdatesLHS :: (Int, Tensor) -> [((Int, Int), (Int, Int))]
p1UpdatesLHS (vsIDX, (Tensor pointersMap)) = updates
  where
    ps = Map.toAscList pointersMap
    updates = [ ((vsIDX, pIDX), (i, j)) | (pIDX, (Pointer (i,j) a)) <- ps, a==Up||a==Down]


p1UpdatesRHSOneElem :: (Int, Tensor) -> [((Int, Int), (Int, Int))]
p1UpdatesRHSOneElem (vsIDX, (Tensor pointersMap)) = updates
  where
    ps = Map.toAscList pointersMap
    updates = [ ((i,j), (k,l)) |
                ((_, (Pointer (i,j) a )):ys) <- tails ps,
                (_, (Pointer (k,l) b )) <- ys,
                a==Up||a==Down,
                b==Up||b==Down,
                a /= b
                                                       ]
p1UpdatesRHS :: ((Int, Tensor), (Int, Tensor)) -> [((Int, Int), (Int, Int))]
p1UpdatesRHS (a, b) = l ++ r
  where
    l = p1UpdatesRHSOneElem (a)
    r = p1UpdatesRHSOneElem (b)

wipeTensor :: Tensors -> Int -> Tensors
wipeTensor (Tensors tensorsMap) key = Tensors $ Map.insert key (Tensor $ Map.fromList []) tensorsMap

wipeTensors :: Tensors -> [Int] -> Tensors
wipeTensors tensors vsIDXs
  | length vsIDXs == 1 = newTensors
  | otherwise = wipeTensors newTensors idxs
  where
    (idx:idxs) = vsIDXs
    newTensors = wipeTensor tensors idx

getTensor :: Tensors -> Int -> Maybe Tensor
getTensor (Tensors tensorsMap) vsIDX = Map.lookup vsIDX tensorsMap

justPointersMap (Tensor pointersMap) = Just pointersMap

getPointersMap :: Tensors -> Int -> Maybe (Map Int Pointer)
getPointersMap tensors vsIDX = pointersMap_
  where
    maybeTensor = getTensor tensors vsIDX
    pointersMap_ = maybeTensor >>= justPointersMap

updatePointerAt :: Maybe Pointer -> (Int, Int) -> Maybe Pointer
updatePointerAt (Just (Pointer _ edgeType)) (i, j) = Just (Pointer (i, j) edgeType)

tensorsMap :: Tensors -> Map Int Tensor
tensorsMap (Tensors tensorsMap) = tensorsMap

rewire :: Tensors -> ((Int, Int), (Int, Int)) -> Maybe Tensors
rewire tensors ((vsIDX, pIDX), newVals) = newTensors
  where
    maybePointersMap = getPointersMap tensors vsIDX
    newPointersMap = case maybePointersMap of
      Nothing -> Nothing
      Just pointersMap -> case updatePointerAt (Map.lookup pIDX pointersMap) newVals of
        Nothing -> Nothing
        Just pointer -> Just $ Map.insert pIDX pointer pointersMap

    newTensor = case newPointersMap of
      Nothing -> Nothing
      Just pointersMap -> Just $ Tensor pointersMap

    newTensors = case newTensor of
      Nothing -> Nothing
      Just tensor -> Just $ Tensors $ Map.insert vsIDX tensor $ tensorsMap tensors

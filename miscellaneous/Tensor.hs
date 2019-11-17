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
keylessMapToList :: Map k a -> [a]
pointers :: Tensor -> [Pointer]
tensorsList :: Tensors -> [Tensor]
nonGluonPointers :: Tensor -> [Pointer]
mappedKeys :: Tensor -> [(Int, Int)]
uniqueVsKeys :: Tensor -> [Int]
tensorPairs :: Tensors -> [(Tensor, Tensor)]
tensorMapPairs :: Tensors -> [((Int, Tensor), (Int, Tensor))]
wipeTensor :: Tensors -> Int -> Tensors
wipeTensors :: Tensors -> [Int] -> Tensors
getTensor :: Tensors -> Int -> Maybe Tensor
justPointersMap :: Tensor -> Maybe (Map Int Pointer)
getPointersMap :: Tensors -> Int -> Maybe (Map Int Pointer)
updatePointerAt :: Maybe Pointer -> (Int, Int) -> Maybe Pointer
pullTensorsMap :: Tensors -> Map Int Tensor
rewireSingle :: ((Int, Int), (Int, Int)) -> Maybe Tensors -> Maybe Tensors
rewireDual :: Tensors -> ((Int, Int), (Int, Int)) -> Maybe Tensors
rewire :: Maybe Tensors -> [((Int,Int),(Int,Int))] -> Maybe Tensors


isGluon (Pointer _ Gluon) = True
isGluon _ = False

keylessMapToList m = [a | (_, a) <- Map.toAscList m]

pointers (Tensor pointersMap) = keylessMapToList pointersMap

tensorsList (Tensors tensorsMap) = keylessMapToList tensorsMap

nonGluonPointers t = Prelude.filter (not . isGluon) $ pointers t

mappedKeys t = [ (i, j) | (Pointer (i,j) _) <- pointers t]

uniqueVsKeys t = nub [ i | (i, _)<- mappedKeys t]

tensorPairs (Tensors tensorsMap) =
  [ (x,y) | (x:ys) <- tails $ keylessMapToList tensorsMap, y <- ys ]

tensorMapPairs (Tensors tensorsMap) =
  [ (x,y) | (x:ys) <- tails $ Map.toAscList tensorsMap, y <- ys ]


wipeTensor (Tensors tensorsMap) key = Tensors $
  Map.adjust (\(Tensor _) -> (Tensor $ Map.fromList [])) key tensorsMap

wipeTensors tensors vsIDXs
  | length vsIDXs == 1 = newTensors
  | otherwise = wipeTensors newTensors idxs
  where
    (idx:idxs) = vsIDXs
    newTensors = wipeTensor tensors idx

getTensor (Tensors tensorsMap) vsIDX = Map.lookup vsIDX tensorsMap

justPointersMap (Tensor pointersMap) = Just pointersMap

getPointersMap tensors vsIDX = pointersMap_
  where
    maybeTensor = getTensor tensors vsIDX
    pointersMap_ = maybeTensor >>= justPointersMap

updatePointerAt (Just (Pointer _ edgeType)) (i, j) = Just (Pointer (i, j) edgeType)
updatePointerAt _ _ = Nothing

pullTensorsMap (Tensors tensorsMap) = tensorsMap

rewireSingle ((vsIDX, pIDX), newVals) (Just tensors) = newTensors
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
      Just tensor -> Just $ Tensors $ Map.insert vsIDX tensor $ pullTensorsMap tensors
rewireSingle _ _ = Nothing

rewireDual tensors (here, there) = rewireSingle (there, here) (rewireSingle (here, there) (Just tensors))

rewire (Just tensors) links
  | length links == 1 = newTensors
  | otherwise = rewire newTensors ls
  where
    (l:ls) = links
    newTensors = rewireDual tensors l
rewire _ _ = Nothing

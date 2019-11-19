module Tensor  where
import EdgeSpaces
--data EdgeType = Up | Down | Gluon deriving (Show, Eq)
import Data.Map.Strict as Map
import Data.List
data Newness a = Original a | New a deriving (Show)
data Pointer = Pointer (Int, Int) EdgeType deriving (Show, Eq)
data Tensor = Tensor (Map Int Pointer) deriving (Show, Eq)
data Tensors = Tensors (Map Int Tensor) deriving (Show)
data VectorSpace poly = TensorProduct poly Tensors deriving (Show)

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
justPointersMap :: Tensor -> Maybe (Map Int Pointer)
getPointersMap :: Int -> Tensors -> Maybe (Map Int Pointer)
updatePointer :: (Int, Int) -> Pointer -> Maybe Pointer
pullTensorsMap :: Tensors -> Map Int Tensor
rewireSingle :: ((Int, Int), (Int, Int)) -> Tensors -> Maybe Tensors
rewireDual :: ((Int, Int), (Int, Int)) -> Tensors -> Maybe Tensors

getTensor :: Int -> Tensors -> Maybe Tensor
wipeTensor :: Tensors -> Int -> Tensors
wipeTensors :: Tensors -> [Int] -> Tensors
rewire :: [((Int,Int),(Int,Int))] -> Tensors -> Maybe Tensors



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

getTensor vsIDX (Tensors tensorsMap) = Map.lookup vsIDX tensorsMap

justPointersMap (Tensor pointersMap) = Just pointersMap

getPointersMap vsIDX tensors = (getTensor vsIDX tensors) >>= justPointersMap

updatePointer (i, j) (Pointer _ edgeType) = Just (Pointer (i, j) edgeType)

pullTensorsMap (Tensors tensorsMap) = tensorsMap

rewireSingle ((vsIDX, pIDX), newVals) tensors = newTensors
  where

    oldPointersMap = return tensors >>= getPointersMap vsIDX

    newPointer = oldPointersMap >>= Map.lookup pIDX >>= updatePointer newVals

    newPointersMap = (Map.insert pIDX) <$> newPointer <*> oldPointersMap

    buildTensor pointersMap = Just $ Tensor pointersMap

    newTensor = newPointersMap >>= buildTensor

    insertTensor tensors vsIDX tensor = Just $ Tensors $ Map.insert vsIDX tensor $ pullTensorsMap tensors

    newTensors = newTensor >>= insertTensor tensors vsIDX

rewireDual (here, there) tensors = return tensors >>=
  rewireSingle (here, there) >>=
  rewireSingle (there, here)

rewire links tensors
  | length links == 1 = newTensors
  | otherwise = newTensors >>= rewire otherLinks
  where
    (link:otherLinks) = links
    newTensors = return tensors >>= rewireDual link

mapFromEdges :: [Edge] -> Map Int Tensor
mapFromEdges edges = mapIntToTensor
  where
  indices = internalIndices edges
  mapList = [ (i, (Tensor (fromList []))) | i <- indices ]
  mapIntToTensor = fromList mapList

tensorsFromEdges :: [Edge] -> Tensors
tensorsFromEdges edges = Tensors $ mapFromEdges edges

vectorSpaceFromEdgeSpace :: EdgeSpace poly -> VectorSpace poly
vectorSpaceFromEdgeSpace (EdgeSpace poly edges) = (TensorProduct poly (tensorsFromEdges edges))

--insertPointer :: []

--rewireLinksFromEdgeList :: [Edge] -> [(Int, Int), (Int, Int)]

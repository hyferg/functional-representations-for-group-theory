module Tensor  where

import Data.Maybe
import Prelude
import Data.Map.Strict as Map
import Data.List

import EdgeSpaces
-- maybe make (Int, Int) index pair a data
data Newness a = Original a | New a deriving (Show)
data Pointer = Pointer (Int, Int) EdgeType deriving (Show, Eq)
data Tensor = Tensor (Map Int Pointer) deriving (Show, Eq)
data Tensors = Tensors (Map Int Tensor) deriving (Show)
data VectorSpace poly = TensorProduct poly Tensors deriving (Show)
data Index = Index (Int, Int) deriving (Show)
data Link = Link ((Int, Int), (Int, Int)) deriving (Show)

class State s where
  (-:) :: s a -> (a -> a) -> s a

instance State Newness where
  (Original x) -: f = New (f x)
  (New x) -: _ = New (x)

class Invert a where
  invert :: a -> a

instance Invert EdgeType where
  invert Up = Down
  invert Down = Up
  invert Gluon = Gluon

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
pullTensorsMap :: Tensors -> Map Int Tensor
rewireSingle :: ((Int, Int), (Int, Int)) -> Tensors -> Maybe Tensors
rewireDual :: ((Int, Int), (Int, Int)) -> Tensors -> Maybe Tensors

getTensor :: Int -> Tensors -> Maybe Tensor
wipeTensor :: Tensors -> Int -> Tensors
wipeTensors :: Tensors -> [Int] -> Tensors
rewire :: [((Int,Int),(Int,Int))] -> Tensors -> Maybe Tensors

updatePointer :: (Int, Int) -> Pointer -> Maybe Pointer
constructEmpty :: [Edge] -> Int -> Map Int Tensor
adjustPointer :: (Pointer -> Maybe Pointer) -> (Int, Int) -> Map Int Tensor
  -> Maybe (Map Int Tensor)
-- returns the internal index of the pointer and the new map
addPointer :: Int -> Pointer -> Map Int Tensor
  -> Maybe (Int, Map Int Tensor)

edgeTypes :: Map Int Pointer -> [EdgeType]
edgeTypes mapIntToPointer = [ edgeType | (_, Pointer _ edgeType) <- toList mapIntToPointer]


matchPatternWithMapIndex edgePattern mapIntToTensor = out
  where
  out = do
    matchTensor <- matchSingle edgePattern mapIntToTensor
    rotateN <- matchRotate matchTensor edgePattern
    y <- Just $ rotate rotateN $ points $ snd matchTensor
    out <- Just $ zip edgePattern y
    Just out


--points :: Tensor -> [(Int, Int)]
points (Tensor mapIntToPointer) = [ a | (_, Pointer a _)<- toList mapIntToPointer]

matchRotate :: (Int, Tensor) -> [Edge] -> Maybe Int
matchRotate (idx, (Tensor mapIntToPointer)) edges
  | length matches /=0 = Just $ head matches
  | otherwise = Nothing
  where

  targetEdgeTypes = [ edgeType | (Edge edgeType (Node _ _, Node _ _)) <- edges]

  p = zip [0,1..] (cyclicPermutations $ edgeTypes mapIntToPointer)
  matches = [ i | ((i, a), b)<- zip p (repeat targetEdgeTypes),
              a == b]

matchSingle :: [Edge] -> Map Int Tensor -> Maybe (Int, Tensor)
matchSingle edges mapIntToTensor
  | length hits == 0 = Nothing
  | otherwise = Just $ head hits
  where

  targetSize = length edges

  targetEdgeTypes = [ edgeType | (Edge edgeType (Node _ _, Node _ _)) <- edges]


  cycleMatch targetList list
    | length matches >=1 = True
    | otherwise = False
    where
    matches = [ True | (a, b) <- zip (cyclicPermutations targetList) (repeat $ list),
                a == b ]

  hits = [(i, (Tensor mapIntToPointer)) |
          (i, (Tensor mapIntToPointer)) <- (toList mapIntToTensor),
          (size mapIntToPointer)==targetSize,
          cycleMatch (targetEdgeTypes) (edgeTypes mapIntToPointer) ]


cyclicPermutations :: [a] -> [[a]]
cyclicPermutations list = [ rotate w list | w <- [0..(-1 + length list)] ]

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (Prelude.drop n (cycle xs)) xs

-- find the links that go from the new subgraph to the larger graph
bridgeIELinks :: [Link] -> [Link]
bridgeIELinks links = [ Link ((i,ii),(j,jj)) | (Link ((i,ii),(j,jj))) <- links,
                        i<0 && j>0 || i>0 && j<0
                        ]

addEdges :: [Edge] -> ([Link], Map Int Tensor) -> Maybe ([Link], Map Int Tensor)
addEdges edges (links, mapIntToTensor)
  | length edges == 1 = linksNewMap
  | otherwise = linksNewMap >>= addEdges otherEdges
  where
    (edge:otherEdges) = edges
    linksNewMap = return (links, mapIntToTensor) >>= addEdge edge

-- builds a map from list of edges
-- additionally returns a list of tuple links to index the map structure
addEdge :: Edge -> ([Link], Map Int Tensor) -> Maybe ([Link], Map Int Tensor)
addEdge (Edge edgeType (Node nodeTypei i, Node nodeTypej j)) (links, mapIntToTensor) = out
  where
    pointer1 = (Pointer (j,999) edgeType)
    pointer2 = (Pointer (i,999) (invert edgeType))
    updatePointerB b (Pointer (a,_) edgeType) = Just $ (Pointer (a,b) edgeType)

    outTuple a b = Just (a, b)

    out = do
      (k1,map1) <- addPointer i pointer1 mapIntToTensor
      (k2,map2) <- addPointer j pointer2 map1
      map3 <- adjustPointer (updatePointerB k2) (i,k1) map2
      map4 <- adjustPointer (updatePointerB k1) (j,k2) map3
      link <- Just $ Link ((i, k1),(j, k2))
      outTuple (link:links) map4


constructEmpty edges firstIndex = y
  where
    idxs = anyIndices edges
    y = Map.fromList [ (
                         (if idx<0 then idx else firstIndex-1+idx),
                         (Tensor (Map.fromList [])))| idx <- idxs]



adjustPointer f (tIDX, pIDX) mapIntToTensor = out
  where
  out = do
    tensor <- Map.lookup tIDX mapIntToTensor
    newTensor <-  (\(Tensor mapIntToPointer) -> Just $ Tensor $ Map.update (f) pIDX mapIntToPointer) tensor
    newTensors <- (\tensor -> Just $ Map.insert tIDX tensor mapIntToTensor) newTensor
    Just newTensors


addPointer key pointer mapIntToTensor
  | internalKey /= Nothing && newTensors /= Nothing = Just out
  | otherwise = Nothing
  where
    tensor = Map.lookup key mapIntToTensor
    map = tensor >>= (\(Tensor map) -> Just map)
    sizeOfMapIntToPointer = map >>= (\map -> Just $ Map.size map)
    internalKey = sizeOfMapIntToPointer

    -- put the pointer in the map with key of the size of the map
    -- the map should be zero indexed so this is kind of like appending

    newMap = ((flip Map.insert) pointer) <$> internalKey <*> map
    newTensor = newMap >>= (\map -> Just $ Tensor map)
    newTensors = newTensor >>=
      (\tensor -> Just $ Map.insert key tensor mapIntToTensor)

    out = (fromMaybe 0 internalKey, fromMaybe (fromList []) newTensors)

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


isGluon (Pointer _ Gluon) = True
isGluon _ = False

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

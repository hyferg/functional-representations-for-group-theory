import Prelude
import Data.Maybe
import Tensor
import qualified MathObj.LaurentPolynomial as LP
import EdgeSpaces
import Text.Pretty.Simple (pPrint)
import Data.Map.Strict as Map


edges = [
  Edge Gluon (Node I (-1), Node E (1)),
  Edge Gluon (Node I (-1), Node E (2)),
  Edge Gluon (Node I (-2), Node I (-1)),
  Edge Gluon (Node I (-2), Node E (3)),
  Edge Gluon (Node I (-2), Node E (4))
  ]

t0 = Tensor $ Map.fromAscList $ zip [0,1..] [
   Pointer (0,1) Down ,
   Pointer (0,0) Up ,
   Pointer (1,0) Gluon
  ]

t1 = Tensor $ Map.fromAscList $ zip [0,1..] [
   Pointer (0,2) Gluon ,
   Pointer (1,2) Down ,
   Pointer (1,1) Up
  ]

dumbellTensors = Map.fromAscList $ zip [0,1..] [t0, t1]

p0  = Pointer (9,9) Gluon

xs = constructEmpty edges



addEdges :: [Edge] -> Map Int Tensor -> Maybe (Map Int Tensor)
addEdges edges mapIntToTensor
  | length edges == 1 = newMap
  | otherwise = newMap >>= addEdges otherEdges
  where
    (edge:otherEdges) = edges
    newMap = return mapIntToTensor >>= addEdge edge

--addEdge :: Map Int Tensor -> Edge -> Map Int Tensor

addEdge (Edge edgeType (Node typeI i, Node typeJ j)) mapIntToTensor = outMap
  where
    pointer1 = (Pointer (j,999) edgeType)
    pointer2 = (Pointer (i,999) (invert edgeType))
    updatePointerB b (Pointer (a,_) edgeType) = Just $ (Pointer (a,b) edgeType)


    outMap = do
      (k1,map1) <- addPointer i pointer1 mapIntToTensor
      (k2,map2) <- addPointer j pointer2 map1
      map3 <- adjustPointer (updatePointerB k2) (i,k1) map2
      map4 <- adjustPointer (updatePointerB k1) (j,k2) map3
      Just map4


constructEmpty :: [Edge] -> Map Int Tensor
constructEmpty edges = y
  where
    idxs = anyIndices edges
    y = Map.fromList [ (idx, (Tensor (Map.fromList [])))| idx <- idxs]

pMod (Pointer (a, b) edgeType) = Just $ Pointer (a,0) edgeType
f (Just a) = a
xss = f $addEdge (edges !! 0) xs

--adjustPointer :: (Pointer -> Maybe Pointer) -> (Int, Int) -> Map Int Tensor -> Map Int Tensor
adjustPointer f (tIDX, pIDX) mapIntToTensor = newTensors
  {--
  | newTensors /= Nothing = fromMaybe (fromList []) newTensors
  | otherwise = mapIntToTensor
-}
  where
  tensor = Map.lookup tIDX mapIntToTensor
  newTensor = tensor >>=
    (\(Tensor mapIntToPointer) -> Just $ Tensor $ Map.update (f) pIDX mapIntToPointer)
  newTensors = newTensor >>=
    (\tensor -> Just $ Map.insert tIDX tensor mapIntToTensor)

--addPointer :: Map Int Tensor -> Int -> Pointer -> Maybe (Map Int Pointer)
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


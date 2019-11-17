module SunP1 where
import Tensor
import Data.Map.Strict as Map
import Data.List
import Data.Tree
import MathObj.LaurentPolynomial
{-
The same ordering of Up/Down pointers in two gluon connected tensors
indicates that it a P1 operator. Maybe needs a test to make sure the
tensor is not (Gluon Down Down) etc.
-}
sunP1 :: Tensor -> Tensor -> Bool
sunP1 t1 t2
  | size (f t1) == size (f t2) &&
    size (f t1) == 3 &&
    2 == length [ True | (i, j) <- zip x y, edgeEq i j] = True
  | otherwise = False
  where
    f (Tensor pointersMap) = pointersMap
    x = nonGluonPointers t1
    y = nonGluonPointers t2

sunP1Link :: Tensors -> Maybe ((Int, Tensor), (Int, Tensor))
sunP1Link tensors
  | [] == sunP1Tensors_ = Nothing
  | otherwise = Just $ head sunP1Tensors_
  where
    sunP1Tensors_ =
      [((k1, t1), (k2, t2)) | ((k1, t1), (k2, t2)) <- tensorMapPairs tensors, sunP1 t1 t2]

p1UpdatesLHS :: ((Int, Tensor), (Int, Tensor)) -> Maybe [((Int, Int, EdgeType), (Int, Int, EdgeType))]
p1UpdatesLHS ( (vs0, (Tensor p0)), (vs1, (Tensor p1)) )
  | [] == updates = Nothing
  | otherwise = Just updates
  where
    ps0 = Map.toAscList p0
    ps1 = Map.toAscList p1
    updates = [ ((vs0, p0IDX, b), (vs1, p1IDX, a)) |
                (p0IDX, (Pointer _ a)) <- ps0, a==Up||a==Down,
                (p1IDX, (Pointer _ b)) <- ps1, b==Up||b==Down,
                a /= b]

p1UpdatesRHSOneElem :: (Int, Tensor) -> Maybe [((Int, Int, EdgeType), (Int, Int, EdgeType))]
p1UpdatesRHSOneElem (_, (Tensor pointersMap))
  | [] == updates = Nothing
  | otherwise = Just updates
  where
    ps = Map.toAscList pointersMap
    updates = [ ((i,j,b), (k,l,a)) |
                ((_, (Pointer (i,j) a )):ys) <- tails ps,
                (_, (Pointer (k,l) b )) <- ys,
                a==Up||a==Down,
                b==Up||b==Down,
                a /= b
                                                       ]
p1UpdatesRHS :: ((Int, Tensor), (Int, Tensor)) -> Maybe [((Int, Int, EdgeType), (Int, Int, EdgeType))]
p1UpdatesRHS (a, b) = (++) <$> l <*> r
  where
    l = p1UpdatesRHSOneElem (a)
    r = p1UpdatesRHSOneElem (b)

-- TODO prune empty tensors
decomposeP1 :: Tensors -> [VectorSpace]
decomposeP1 tensors = decomposed
  where
    updatesLHS = return tensors >>= sunP1Link >>= p1UpdatesLHS
    updatesRHS = return tensors >>= sunP1Link >>= p1UpdatesRHS
    maybeIdxs = return tensors >>= sunP1Link >>= vsIDXsToWipe
    scrubbedTensors = case maybeIdxs of
                        Nothing -> Nothing
                        Just idxs -> Just (wipeTensors idxs tensors)

    lhs = rewire scrubbedTensors updatesLHS
    lhsPoly = fromCoeffs [1]

    rhs = rewire scrubbedTensors updatesRHS
    rhsPoly = fromShiftCoeffs (-1) [-1]
    decomposed = case lhs of
                   Nothing -> []
                   Just lhsTensors -> case rhs of
                     Nothing -> []
                     Just rhsTensors -> [
                       TensorProduct lhsPoly lhsTensors,
                       TensorProduct rhsPoly rhsTensors
                       ]

buildNode :: VectorSpace -> ((T Int), [VectorSpace])
buildNode (TensorProduct poly tensors) = (poly, decomposeP1 tensors)

-- TODO
--workNode :: (T Int -> [T Int] -> T Int)

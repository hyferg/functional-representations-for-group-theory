import Graph
import FlatGraph
import Decompositions
import AddGraph
import MathObj.LaurentPolynomial as LP
import Data.Maybe

--build :: (FlatGraph g) => (g -> Maybe g) -> VectorSpace Poly g
build someGraphOn
  | Just g <- someGraphOn emptyGraph = VS plusOne g
  | otherwise = VS zero emptyGraph

decompose :: (FlatGraph g) =>
  VectorSpace Poly g ->
  Either (Poly, [VectorSpace Poly g])
                (VectorSpace Poly g)
decompose (VS poly g)
  | length out >= 1 = Left $ head out
  | otherwise = Right $ VS poly g
  where
    gluons = gluonEdges $ allEdges_ g
    decompIn = [ (edge, (VS poly g)) | edge <- gluons ]
    out = catMaybes $ map sunP1Decomposition decompIn

gluonEdges :: [Edge] -> [Edge]
gluonEdges edges = [ e | e <- edges, e =-= (Edge 0 [] G)]

--input :: (FlatGraph g) => VectorSpace Poly g
input = build pill
out = decompose input


-- (Left _):Either implies that some strat worked in the bind chain
-- (Right _):Either means no strat worked
{-


applyStrats :: (FlatGraph g) => g -> f -> Either [VectorSpace Poly g] g
applyStrats g f = return g >>= f

strat :: (FlatGraph g) => g -> Either [VectorSpace Poly g] g
  | otherwise = Nothing
  where

out = do
  g0 <- Decompositions.merge $ pill emptyGraph
  eg <- Right $ (allEdges_ g0) !! 1
  sunP1Decomposition eg g0



gluonEdge :: [Edge] -> Maybe Edge
gluonEdge edges
  | 1 <= length gluons = Just $ head gluons
  | otherwise = Nothing
  where
    gluons = gluonEdges edges

buildNode :: (FlatGraph g) =>
  (VectorSpace Poly g) -> (Poly, [VectorSpace Poly g])
buildNode (VS poly g)
  | [] == vs = (poly, [])
  where
    vs = []
    strats = [sunP1Decomposition]
    out = do
      eg <- gluonEdge (allEdges_ g)
      target <- return $ decompEdge (eg, g)
      ds <- return $ map target strats
      return [ v | Right a <- ds]


-- TODO fix Right rule []
decompEdge :: (FlatGraph g) =>
  (Edge, g) ->
  (Edge -> g -> (Either [VectorSpace Poly g] g)) ->
  [VectorSpace Poly g]
decompEdge (e, g) f
  | (Right _)        <- f e g = []
  | (Left split)     <- f e g = split


-}


--nodes = allNodes_ <$> out
--edges = allEdges_ <$> out

module SunP1 where

import TensorGraph
import EdgeNode

{-
i    k
 ^  v
j    l
returns (i,j,k,l) (nodeIDX(^), nodeIDX(v))
-}
sunP1Corners :: (TensorGraph g) => (Int, Edge) -> g -> Maybe ((Int,Int,Int,Int), (Int,Int))
sunP1Corners (edgeIDX, (Edge _ (a, b))) graph = do
      lhsEdges <- getOrientedEdges a graph
      rhsEdges <- getOrientedEdges b graph
      (Edge _ (_,i)) <- filterOneEdge Down lhsEdges
      (Edge _ (_,j)) <- filterOneEdge Up lhsEdges
      (Edge _ (_,k)) <- filterOneEdge Up rhsEdges
      (Edge _ (_,l)) <- filterOneEdge Down rhsEdges
      return $ ((i,j,k,l),(a,b))

sunP1Edges :: (TensorGraph g) => g -> [(Int, Edge)]
sunP1Edges graph = [ ie | ie <- gluonEdges graph, isSunP1 ie graph]

isSunP1 :: (TensorGraph g) => (Int, Edge) -> g -> Bool
isSunP1 (_, (Edge edgeType (lhsNodeIDX, rhsNodeIDX))) graph
  | Just bool <- out = bool
  | otherwise = False
  where
    out = do
      lhs <- classifyNode lhsNodeIDX graph
      rhs <- classifyNode rhsNodeIDX graph
      nodesFormP1 <- Just $ (lhs, rhs) == (Clock, Clock) || (lhs, rhs) == (AntiClock, AntiClock)
      edgeIsGluon <- Just $ edgeType == Gluon
      return $ edgeIsGluon && nodesFormP1

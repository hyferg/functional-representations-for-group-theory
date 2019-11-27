module SunP1 where

import TensorGraph
import EdgeNode

import MathObj.LaurentPolynomial as LP
data VectorSpace graph = VectorSpace (LP.T Int) graph

--decomposeP1 :: (TensorGraph g) => g -> Maybe (VectorSpace, VectorSpace)
decomposeP1 graph = do
  (edgeIDX, edge) <- maybeSunP1Edge graph
  lhsGraph <- workLHS (edgeIDX, edge) graph
  rhsGraph <- workRHS (edgeIDX, edge) graph
  return (
    ( fromCoeffs [1], lhsGraph ),
    ( fromShiftCoeffs (-1) [1], rhsGraph )
    )

maybeSunP1Edge :: (TensorGraph g) => g -> Maybe (Int, Edge)
maybeSunP1Edge graph
  | length edges == 0 = Nothing
  | otherwise = Just $ head edges
  where
    edges = sunP1Edges graph


workLHS :: (TensorGraph g) => (Int, Edge) -> g -> Maybe g
workLHS (gluonIDX, edge) graph = do
  corners <- sunP1Corners (gluonIDX, edge) graph
  return $ sunP1LHSEdges corners (deleteEdge gluonIDX graph)

workRHS :: (TensorGraph g) => (Int, Edge) -> g -> Maybe g
workRHS (gluonIDX, edge) graph = do
  corners <- sunP1Corners (gluonIDX, edge) graph
  return $ sunP1RHSEdges corners (deleteEdge gluonIDX graph)

sunP1RHSEdges :: (TensorGraph g) => (
  ((Int,Int),(Int,Int),(Int,Int),(Int,Int)),
   (Int, Int)
  ) -> g -> g
sunP1RHSEdges (((i,ii),(j,jj),(k,kk),(l,ll)),(_,_)) graph = gg
  where
    e1 = Edge Up (i,j)
    e2 = Edge Up (l,k)

    prunedGraph = deleteEdges [ii,jj,kk,ll] graph

    (e1idx, g1) = addEdge e1 prunedGraph
    (e2idx, g2) = addEdge e2 g1


    gg = replaceEdgeInNode (kk, e2idx) k $
         replaceEdgeInNode (ll, e2idx) l $
         replaceEdgeInNode (jj, e1idx) j $
         replaceEdgeInNode (ii, e1idx) i g2

sunP1LHSEdges :: (TensorGraph g) => (
  ((Int,Int),(Int,Int),(Int,Int),(Int,Int)),
   (Int, Int)
  ) -> g -> g
sunP1LHSEdges (((i,ii),(j,jj),(k,kk),(l,ll)),(_,_)) graph = gg
  where
    e1 = Edge Up (i,k)
    e2 = Edge Up (l,j)

    prunedGraph = deleteEdges [ii,jj,kk,ll] graph

    (e1idx, g1) = addEdge e1 prunedGraph
    (e2idx, g2) = addEdge e2 g1


    gg = replaceEdgeInNode (ll, e2idx) l $
         replaceEdgeInNode (jj, e2idx) j $
         replaceEdgeInNode (kk, e1idx) k $
         replaceEdgeInNode (ii, e1idx) i g2

{-
i    k
 ^  v
j    l
returns (i,j,k,l) (nodeIDX(^), nodeIDX(v))
-}
--sunP1Corners :: (TensorGraph g) => (Int, Edge) -> g -> Maybe ((Int,Int,Int,Int), (Int,Int))
sunP1Corners (_, (Edge _ (a, b))) graph = do
      lhsEdges <- getOrientedEdges a graph
      rhsEdges <- getOrientedEdges b graph
      (ii, Edge _ (_,i)) <- filterOneEdge Down lhsEdges
      (jj, Edge _ (_,j)) <- filterOneEdge Up lhsEdges
      (kk, Edge _ (_,k)) <- filterOneEdge Up rhsEdges
      (ll, Edge _ (_,l)) <- filterOneEdge Down rhsEdges
      return $ (((i,ii),(j,jj),(k,kk),(l,ll)),(a,b))

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

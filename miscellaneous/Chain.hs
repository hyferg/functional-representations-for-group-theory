module Chain where

import TensorGraph
import EdgeNode

--decomposeChain :: (TensorGraph g) => (Int, Edge) -> g -> Maybe [VectorSpace]

--decomposeChain :: (TensorGraph g) => (Int, Edge) -> g -> Maybe [VectorSpace]

--decomposeChain :: (TensorGraph g) => (Int, Edge) -> g -> maybe VectorSpace
decomposeChain (edgeIDX, edge) graph = do
  info <- chainInfo (edgeIDX, edge) graph
  return $ modifyChain info graph


modifyChain :: (TensorGraph g) => (Int, Int, Int, Int, Int) -> g -> g
modifyChain (node1IDX, e1IDX, idx, e2IDX, node2IDX) graph = graphFinal
  where
    newEdge = Edge Gluon (node1IDX, node2IDX)

    prunedGraph = deleteEdges [e1IDX, e2IDX] graph

    (newEdgeIDX, graphWithNewEdge) = addEdge newEdge prunedGraph

    graphFinal = replaceEdgeInNode (e2IDX, newEdgeIDX) node2IDX $
                 replaceEdgeInNode (e1IDX, newEdgeIDX) node1IDX graphWithNewEdge


-- determines if and edge is part of a chain as described...
-- (node1) --edge1-- (ggNode) --edge2-- (node2)
-- returns (node1IDX, edge1IDX, ggNodeIDX, edge2IDX, node2IDX)
chainInfo :: (TensorGraph g) => (Int, Edge) -> g -> Maybe (Int,Int,Int,Int,Int)
chainInfo (edgeIDX, Edge edgeType (lhsNodeIDX, rhsNodeIDX)) graph = do
      idx <- ggNodeIDX (edgeIDX, Edge edgeType (lhsNodeIDX, rhsNodeIDX)) graph
      edges <- getOrientedEdges idx graph
      (e1IDX, Edge _ e1NodeIDXs) <- Just $ edges !! 0
      (e2IDX, Edge _ e2NodeIDXs) <- Just $ edges !! 1
      node1IDX <- Just $ if fst e1NodeIDXs == idx then snd e1NodeIDXs else fst e1NodeIDXs
      node2IDX <- Just $ if fst e2NodeIDXs == idx then snd e2NodeIDXs else fst e2NodeIDXs
      return (node1IDX, e1IDX, idx, e2IDX, node2IDX)


ggNodeIDX :: (TensorGraph g) => (Int, Edge) -> g -> Maybe Int
ggNodeIDX (_, (Edge edgeType (lhsNodeIDX, rhsNodeIDX))) graph
  | Just (GG, _) <- out, isGluon = Just lhsNodeIDX
  | Just (_, GG) <- out, isGluon = Just rhsNodeIDX
  | otherwise = Nothing
  where
    isGluon = edgeType == Gluon
    out = do
      lhs <- classifyNode lhsNodeIDX graph
      rhs <- classifyNode rhsNodeIDX graph
      return (lhs, rhs)

isChain :: (TensorGraph g) => (Int, Edge) -> g -> Bool
isChain (_, (Edge edgeType (lhsNodeIDX, rhsNodeIDX))) graph
  | Just bool <- out = bool
  | otherwise = False
  where
    out = do
      lhs <- classifyNode lhsNodeIDX graph
      rhs <- classifyNode rhsNodeIDX graph
      aNodeFormsGG <- Just $ lhs == GG || rhs == GG
      edgeIsGluon <- Just $ edgeType == Gluon
      return $ edgeIsGluon && aNodeFormsGG

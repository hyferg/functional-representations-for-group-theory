module Bubble where

import TensorGraph
import EdgeNode

decomposeBubble :: (TensorGraph g) => (Int, Edge) -> g -> Maybe g
decomposeBubble (edgeIDX, edge) graph = do
  info <- bubbleInfo (edgeIDX, edge) graph
  return $ modifyBubble info graph

modifyBubble :: (TensorGraph g) => (Int, Int) -> g -> g
modifyBubble (e1IDX, e2IDX) graph = deleteEdges [e1IDX, e2IDX] graph

-- is an edge a part of a two edge bubble?
-- ... --edge2-- (node1) --edge1-- (node2) --edge2-- ...
-- returns (edge1IDX, edge2IDX)
bubbleInfo :: (TensorGraph g) => (Int, Edge) -> g -> Maybe (Int, Int)
bubbleInfo (edgeIDX, Edge _ (lhsNodeIDX, rhsNodeIDX)) graph
  | Just (GG, GG) <- nodeClassifications,
    Just otherIDX <- idx = Just (edgeIDX, otherIDX)
  | otherwise = Nothing
  where
    idxEq (a, _) (b, _) = a == b
    nodeClassifications = do
      lhsNode <- classifyNode lhsNodeIDX graph
      rhsNode <- classifyNode rhsNodeIDX graph
      return (lhsNode, rhsNode)

    idx = do
      lhsEdges <- getOrientedEdges lhsNodeIDX graph
      rhsEdges <- getOrientedEdges rhsNodeIDX graph
      (otherIDX, _) <- Just $ head [ e1 | e1 <- lhsEdges, e2 <- rhsEdges,
                                     idxEq e1 e2,
                                     not $ idxEq e1 (edgeIDX, True)  ]

      return otherIDX

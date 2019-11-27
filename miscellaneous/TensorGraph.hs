--{-# LANGUAGE MultiParamTypeClasses #-}
-- FunctionalDependencies

module TensorGraph where
import EdgeNode

class TensorGraph g where
  emptyGraph :: g
  addEdge :: Edge -> g -> (Int, g)
  addNodeIfNotExists :: Int -> Node -> g -> g
  addEdgeToNode :: Int -> Int -> g -> g
  addNode :: NodeType -> g -> (Int, g)
  classifyNode :: Int -> g -> Maybe NodeType
  setNodeType :: Int -> NodeType -> g -> g
  getAllEdgesOfType :: EdgeType -> g -> [(Int, Edge)]

  deleteEdge :: Int -> g -> g
  removeEdgeFromNode :: Int -> Int -> g -> g

  getAllNodeIDXs :: g -> [Int]
  getEdgeType :: Int -> g -> Maybe EdgeType

  -- IMPORTANT : make sure your new edge is already a part of the graph
  -- otherwise nothing will happen
  replaceEdgeInNode :: (Int, Int) -> Int -> g -> g

  --getNode :: Int -> g -> Maybe Node
  --getEdges :: Int -> g -> Maybe [Edge]
  getOrientedEdges :: Int -> g -> Maybe [Edge]

gluonEdges :: (TensorGraph g) => g -> [(Int, Edge)]
gluonEdges graph = [ ie | ie <- getAllEdgesOfType Gluon graph ]

addNodes :: (TensorGraph g) => [NodeType] -> g -> g
addNodes nodeTypes graph
  | length nodeTypes == 0 = graph
  | otherwise = addNodes otherNodeTypes newGraph
  where
    (nodeType:otherNodeTypes) = nodeTypes
    (_, newGraph) = addNode nodeType graph

addEdgeToNodes :: (TensorGraph g) => Int -> [Int] -> g -> g
addEdgeToNodes edgeIDX nodeIDXs graph
  | length nodeIDXs == 0 = graph
  | otherwise = addEdgeToNodes edgeIDX otherNodeIDXs newGraph
  where
    (nodeIDX:otherNodeIDXs) = nodeIDXs
    newGraph = addEdgeToNode edgeIDX nodeIDX graph

buildGraph :: (TensorGraph g) => [Edge] -> g -> g
buildGraph edges graph
  | length edges == 0 = graph
  | otherwise = buildGraph otherEdges newGraph
  where
    doubleToList (a, b) = [a, b]
    emptyNode = Node Unidentified []

    ((Edge edgeType nodeIDXs):otherEdges) = edges
    (edgeIDX, g) = addEdge (Edge edgeType nodeIDXs) graph
    -- TODO refactor this code
    g1 = addNodeIfNotExists (fst nodeIDXs) emptyNode g
    g2 = addNodeIfNotExists (snd nodeIDXs) emptyNode g1
    newGraph = addEdgeToNodes edgeIDX (doubleToList nodeIDXs) g2

classifyAndSetNode :: (TensorGraph g) => Int -> g -> g
classifyAndSetNode nodeIDX graph
  | Just newGraph <- maybeNewGraph = newGraph
  | otherwise = graph
  where
    maybeNewGraph = do
      nodeType <- classifyNode nodeIDX graph
      return $ setNodeType nodeIDX nodeType graph

classifyAndSetNodes :: (TensorGraph g) => [Int] -> g -> g
classifyAndSetNodes nodeIDXs graph
  | length nodeIDXs == 0 = graph
  | otherwise = classifyAndSetNodes otherNodeIDXs newGraph
  where
    (nodeIDX:otherNodeIDXs) = nodeIDXs
    newGraph = classifyAndSetNode nodeIDX graph


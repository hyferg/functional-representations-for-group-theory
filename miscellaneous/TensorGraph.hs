module TensorGraph where
import EdgeNode

import MathObj.LaurentPolynomial as LP
--data VectorSpace = VectorSpace (LP.T Int) 

class TensorGraph g where
  emptyGraph :: g

  -- operations on edges

  addEdge :: Edge -> g -> (Int, g)
  deleteEdge :: Int -> g -> g
  getEdgeType :: Int -> g -> Maybe EdgeType
  getAllEdgesOfType :: EdgeType -> g -> [(Int, Edge)]

  -- operations on nodes

  addNode :: NodeType -> g -> (Int, g)
  addNodeIfNotExists :: Int -> Node -> g -> g
  addEdgeToNode :: Int -> Int -> g -> g
  removeEdgeFromNode :: Int -> Int -> g -> g
  replaceEdgeInNode :: (Int, Int) -> Int -> g -> g
  classifyNode :: Int -> g -> Maybe NodeType
  setNodeType :: Int -> NodeType -> g -> g
  getAllNodeIDXs :: g -> [Int]
  -- IMPORTANT : make sure your new edge is already a part of the graph
  -- otherwise nothing will happen
  getOrientedEdges :: Int -> g -> Maybe [(Int, Edge)]

decomposeRule edgeCheck decompose (edgeIDX, edge) graph
  | edgeCheck (edgeIDX, edge) graph = Just $ decompose (edgeIDX, edge)
  | otherwise = Nothing

-- possibly not efficient if (length > 0) has to check more than 1 element
nextEdgeOfType :: (TensorGraph g) => Int -> EdgeType -> g -> Maybe (Int, Edge)
nextEdgeOfType prevEdgeIDX edgeType graph
  | length rightEdges > 0 = Just $ head rightEdges
  | otherwise = Nothing
  where
    edges = getAllEdgesOfType edgeType graph
    rightEdges = filter (\(edgeIDX, _) -> edgeIDX > prevEdgeIDX) edges

deleteEdges :: (TensorGraph g) => [Int] -> g -> g
deleteEdges edgeIDXs graph
  | length edgeIDXs == 0 = graph
  | otherwise = deleteEdges otherEdgeIDXs newGraph
  where
    (edgeIDX:otherEdgeIDXs) = edgeIDXs
    newGraph = deleteEdge edgeIDX graph

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



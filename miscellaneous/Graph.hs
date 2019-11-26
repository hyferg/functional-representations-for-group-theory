{-# LANGUAGE MultiParamTypeClasses #-}
-- FunctionalDependencies

module Graph where
import EdgeNode
import Data.Map.Strict as Map
import Data.Maybe
data Graph = Graph [Edge] (Map Int Node) deriving (Show)

getAllEdgesOfType :: EdgeType -> Graph -> [(Int, Edge)]
getAllEdgesOfType targetEdgeType (Graph edges _) = [
  (i, (Edge edgeType b)) |
  (i, (Edge edgeType b)) <- zip [0,1..] edges,
  edgeType==targetEdgeType ]

getAllNodeIDXs :: Graph -> [Int]
getAllNodeIDXs (Graph _ mapIntNode) = keys mapIntNode

addEdgeToNode :: Int -> Int -> Graph -> Graph
addEdgeToNode edgeIDX nodeIDX (Graph a mapIntNode) = (Graph a newMapIntNode)
  where
    newMapIntNode = adjust (\node -> appendEdgeIDXtoNode edgeIDX node) nodeIDX mapIntNode

addNodeIfNotExists :: Int -> Node -> Graph -> Graph
addNodeIfNotExists nodeIDX node (Graph edges mapIntNode) = (Graph edges newMapIntNode)
  where
    newMapIntNode = insertWith (\_ oldNode -> oldNode) nodeIDX node mapIntNode

emptyGraph :: Graph
emptyGraph = Graph [] empty

getNode :: Int -> Graph -> Maybe Node
getNode nodeIDX (Graph _ mapIntNode) = Map.lookup nodeIDX mapIntNode

getEdge :: Int -> Graph -> Maybe Edge
getEdge edgeIDX (Graph edges _)
  | length edges <= edgeIDX = Nothing
  | otherwise = Just $ edges !! edgeIDX

addNode :: NodeType -> Graph -> (Int, Graph)
addNode nodeType (Graph edges mapIntNode) = (index, Graph edges newMapIntNode)
  -- TODO assumes the map is zero indexed
  -- catch this maybe?
  where
    index = size mapIntNode
    newMapIntNode = insert index (Node nodeType []) mapIntNode

addEdge :: Edge -> Graph -> (Int, Graph)
addEdge edge (Graph edges nodes) = (index, Graph newEdges nodes)
  -- TODO possibly 'throw error' if the nodes do not exist
  where
    newEdges = edges ++ [edge]
    index = -1 + length newEdges

getEdgeIDXs :: Int -> Graph -> Maybe [Int]
getEdgeIDXs nodeIDX graph = getNode nodeIDX graph >>=
  (\(Node _ edgeIDXs) -> Just edgeIDXs)

getEdgeType :: Int -> Graph -> Maybe EdgeType
getEdgeType edgeIDX graph = getEdge edgeIDX graph >>=
  (\(Edge edgeType _) -> Just edgeType)

getEdgeTypes :: Int -> Graph -> Maybe [EdgeType]
getEdgeTypes nodeIDX graph = maybeEdgeTypes
  where
    maybeEdgeTypes = do
      edgeIDXs <- getEdgeIDXs nodeIDX graph
      edgeTypes <- Just $ catMaybes [ getEdgeType e graph | e <- edgeIDXs]
      out <- if length edgeTypes == length edgeIDXs then
        Just edgeTypes else Nothing
      return out

classifyNode :: Int -> Graph -> Maybe NodeType
classifyNode nodeIDX graph
  | Just edgeTypes <- getEdgeTypes nodeIDX graph
  , isClock edgeTypes = Just Clock
  | Just edgeTypes <- getEdgeTypes nodeIDX graph
  , isAntiClock edgeTypes = Just AntiClock
  | Just edgeTypes <- getEdgeTypes nodeIDX graph
  , isGGG edgeTypes = Just GGG
  | Just _ <- getEdgeTypes nodeIDX graph = Just Unidentified
  | otherwise = Nothing

-- setNodeType :: Int -> NodeType -> Graph -> Graph
-- setNodeType nodeIDX nodeType (Graph edges nodes) = out
--   where

setNodeType :: Int -> NodeType -> Graph -> Graph
setNodeType nodeIDX nodeType (Graph edges mapIntNode) = Graph edges newMapIntNode
  where
    newMapIntNode = adjust
      (\(Node _ b) -> (Node nodeType b)) nodeIDX mapIntNode

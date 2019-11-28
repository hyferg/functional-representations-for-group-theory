{-# LANGUAGE MultiParamTypeClasses #-}
-- FunctionalDependencies

module GraphV2 where
import EdgeNode
import Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Sequence as Seq
import Data.Maybe
data Graph = Graph (Seq.Seq Edge) (Map.Map Int Node) deriving (Show)

-- Note : this will do nothing if the newEdgeIDX is not a part of the graph
replaceEdgeInNode :: (Int, Int) -> Int -> Graph -> Graph
replaceEdgeInNode (targetEdgeIDX, newEdgeIDX) nodeIDX (Graph edges mapIntNode)
  | Just graph <- newGraph = graph
  | otherwise = Graph (Seq.fromList []) Map.empty
  where
    newGraph = do
      node <- getNode nodeIDX (Graph edges mapIntNode)
      newNode <- Just $ replaceEdgeIDXInNode targetEdgeIDX newEdgeIDX node
      newMapIntNode <- Just $ Map.insert nodeIDX newNode mapIntNode
      nodeType <- classifyNode nodeIDX (Graph edges newMapIntNode)
      return $ setNodeType nodeIDX nodeType (Graph edges newMapIntNode)


removeEdgeFromNode :: Int -> Int -> Graph -> Graph
removeEdgeFromNode edgeIDX nodeIDX (Graph edges mapIntNode)
  | Just graph <- newGraph = graph
  | otherwise = Graph edges mapIntNode
  where
    newGraph = do
      node <- getNode nodeIDX (Graph edges mapIntNode)
      prunedNode <- Just $ removeEdgeIDXfromNode edgeIDX node
      newMapIntNode <- Just $ Map.insert nodeIDX prunedNode mapIntNode
      nodeType <- classifyNode nodeIDX (Graph edges newMapIntNode)
      return $ setNodeType nodeIDX nodeType (Graph edges newMapIntNode)

deleteEdge :: Int -> Graph -> Graph
deleteEdge edgeIDX (Graph edges mapIntNode) = (Graph newEdges mapIntNode)
  where
    newEdges = Seq.update edgeIDX (Edge None (-1,-1)) edges

getAllEdgesOfType :: EdgeType -> Graph -> [(Int, Edge)]
getAllEdgesOfType targetEdgeType (Graph edges _) = [
  (i, (Edge edgeType b)) |
  (i, (Edge edgeType b)) <- Prelude.zip [0,1..] (Foldable.toList edges),
  edgeType==targetEdgeType ]

getAllNodeIDXs :: Graph -> [Int]
getAllNodeIDXs (Graph _ mapIntNode) = Map.keys mapIntNode

addEdgeToNode :: Int -> Int -> Graph -> Graph
addEdgeToNode edgeIDX nodeIDX (Graph a mapIntNode) = (Graph a newMapIntNode)
  where
    newMapIntNode = Map.adjust (\node -> appendEdgeIDXtoNode edgeIDX node) nodeIDX mapIntNode

addNodeIfNotExists :: Int -> Node -> Graph -> Graph
addNodeIfNotExists nodeIDX node (Graph edges mapIntNode) = (Graph edges newMapIntNode)
  where
    newMapIntNode = Map.insertWith (\_ oldNode -> oldNode) nodeIDX node mapIntNode

emptyGraph :: Graph
emptyGraph = Graph Seq.empty Map.empty

getNode :: Int -> Graph -> Maybe Node
getNode nodeIDX (Graph _ mapIntNode) = Map.lookup nodeIDX mapIntNode

getEdge :: Int -> Graph -> Maybe Edge
getEdge edgeIDX (Graph edges _)
  | Seq.length edges <= edgeIDX = Nothing
  | otherwise = Just $ Seq.index edges edgeIDX

addNode :: NodeType -> Graph -> (Int, Graph)
addNode nodeType (Graph edges mapIntNode) = (index, Graph edges newMapIntNode)
  -- TODO assumes the map is zero indexed
  -- catch this maybe?
  where
    index = Map.size mapIntNode
    newMapIntNode = Map.insert index (Node nodeType []) mapIntNode

addEdge :: Edge -> Graph -> (Int, Graph)
addEdge edge (Graph edges nodes) = (index, Graph newEdges nodes)
  -- TODO possibly 'throw error' if the nodes do not exist
  where
    newEdges = edges |> edge
    index = -1 + Seq.length newEdges

getEdgeIDXs :: Int -> Graph -> Maybe [Int]
getEdgeIDXs nodeIDX graph = getNode nodeIDX graph >>=
  (\(Node _ edgeIDXs) -> Just edgeIDXs)

getEdgeType :: Int -> Graph -> Maybe EdgeType
getEdgeType edgeIDX graph = getEdge edgeIDX graph >>=
  (\(Edge edgeType _) -> Just edgeType)



orientEdge :: Int -> Edge -> Edge
orientEdge nodeIDX (Edge edgeType (a,b))
  | b == nodeIDX = flipEdge (Edge edgeType (a,b))
  | otherwise = (Edge edgeType (a,b))

getOrientedEdge :: Int -> Int -> Graph -> Maybe (Int, Edge)
getOrientedEdge edgeIDX nodeIDX graph = do
  edge <- getEdge edgeIDX graph
  return $ (edgeIDX, orientEdge nodeIDX edge)

getOrientedEdges :: Int -> Graph -> Maybe [(Int, Edge)]
getOrientedEdges nodeIDX graph = do
  idxs <- getEdgeIDXs nodeIDX graph
  return $  catMaybes [ getOrientedEdge idx nodeIDX graph | idx <- idxs]

getOrientedEdgeType :: Int -> Int -> Graph -> Maybe EdgeType
getOrientedEdgeType edgeIDX nodeIDX graph = do
  edge <- getEdge edgeIDX graph
  (Edge edgeType _) <- Just $ orientEdge nodeIDX edge
  return edgeType

getEdgeTypes :: Int -> Graph -> Maybe [EdgeType]
getEdgeTypes nodeIDX graph = maybeEdgeTypes
  where
    maybeEdgeTypes = do
      edgeIDXs <- getEdgeIDXs nodeIDX graph
      edgeTypes <- Just $ catMaybes [
        getOrientedEdgeType e nodeIDX graph | e <- edgeIDXs ]
      out <- if Foldable.length edgeTypes == Foldable.length edgeIDXs then
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
  | Just edgeTypes <- getEdgeTypes nodeIDX graph
  , isGGChain edgeTypes = Just GG
  | Just _ <- getEdgeTypes nodeIDX graph = Just Unidentified
  | otherwise = Nothing

setNodeType :: Int -> NodeType -> Graph -> Graph
setNodeType nodeIDX nodeType (Graph edges mapIntNode) = Graph edges newMapIntNode
  where
    newMapIntNode = Map.adjust
      (\(Node _ b) -> (Node nodeType b)) nodeIDX mapIntNode

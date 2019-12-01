import Data.Map.Strict as Map hiding (take)
import Data.Maybe
import FlatGraph

data NodeP = NodeP [Eidx] deriving (Show)
data EdgeP = EdgeP [Nidx] EdgeType deriving (Show)
data Graph = Graph (Map Nidx NodeP) (Map Eidx EdgeP) deriving (Show)

emptyGraph :: Graph
emptyGraph = Graph (Map.empty) (Map.empty)

ggg = f emptyGraph

instance FlatGraph Graph where
    getNode_           = getNode
    getEdge_           = getEdge
    freeEdgeIndicesOf_ = freeEdgeIndices
    freeNodeIndicesOf_ = freeNodeIndices
    insertNodes_       = insertNodes
    insertEdges_       = insertEdges
    allNodes_          = allNodes_
    allEdges_          = allEdges_

getNode :: Nidx -> Graph -> Maybe Node
getNode nIDX (Graph min mie) = do
  (NodeP eIDXs) <- Map.lookup nIDX min
  return $ Node nIDX (catMaybes [ getEdge e (Graph min mie) | e <- eIDXs ])

getEdge :: Eidx -> Graph -> Maybe Edge
getEdge eIDX (Graph min mie) = do
  (EdgeP nIDXs edgeType) <- Map.lookup eIDX mie
  return $ Edge eIDX (catMaybes [ getNode n (Graph min mie) | n <- nIDXs]) edgeType

freeEdgeIndices :: Int -> Graph -> [Eidx]
freeEdgeIndices n (Graph _ map)
  | size map == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax map

freeNodeIndices :: Int -> Graph -> [Eidx]
freeNodeIndices n (Graph map _)
  | size map == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax map

insertNodes :: [Node] -> Graph -> Maybe Graph
insertNodes nodes graph
  | length nodes == 0 = Just graph
  | otherwise = graph' >>= insertNodes otherNodes
  where
    (node:otherNodes) = nodes
    graph' = return graph >>= insertNode node

insertEdges :: [Edge] -> Graph -> Maybe Graph
insertEdges edges graph
  | length edges == 0 = Just graph
  | otherwise = graph' >>= insertEdges otherEdges
  where
    (edge:otherEdges) = edges
    graph' = return graph >>= insertEdge edge

allNodes :: Graph -> [Node]
allNodes (Graph mapNode mapEdge) = catMaybes [
  getNode nIDX graph | nIDX <- keys mapNode ]
  where
    graph = (Graph mapNode mapEdge)

allEdges :: Graph -> [Edge]
allEdges (Graph mapNode mapEdge) = catMaybes [
  getEdge eIDX graph | eIDX <- keys mapEdge ]
  where
    graph = (Graph mapNode mapEdge)

----

getEdgeIDXs :: [Edge] -> [Eidx]
getEdgeIDXs edges = [ eIDX | Edge eIDX _ _ <- edges ]

getEdgeIDXsIn :: Node -> [Eidx]
getEdgeIDXsIn (Node _ edges) = getEdgeIDXs edges

insertEdge :: Edge -> Graph -> Maybe Graph
insertEdge (Edge eIDX [Node n1IDX _, Node n2IDX _] eType) (Graph mapNode mapEdge)
  | not $ member eIDX mapEdge = Just graph'
  | otherwise = Nothing
  where
    mapEdge' = Map.insert eIDX (EdgeP [n1IDX, n2IDX] eType) mapEdge
    graph' = Graph mapNode mapEdge'

insertNode :: Node -> Graph -> Maybe Graph
insertNode (Node nIDX edges) (Graph mapNode mapEdge)
  | not $ member nIDX mapNode = Just graph'
  | otherwise = Nothing
  where
    mapNode' = Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode
    graph' = Graph mapNode' mapEdge

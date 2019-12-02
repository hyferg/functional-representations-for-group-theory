module Graph where
import Data.Map.Strict as Map hiding (take)
import Data.Maybe
import FlatGraph
import AddGraph

data NodeP = NodeP [Eidx] deriving (Show)
data EdgeP = EdgeP [Nidx] EdgeType deriving (Show)
data Graph = Graph (Map Nidx NodeP) (Map Eidx EdgeP) deriving (Show)

emptyGraph :: Graph
emptyGraph = Graph (Map.empty) (Map.empty)

instance FlatGraph Graph where
    getNode_           = getNode
    getEdge_           = getEdge
    freeEdgeIndicesOf_ = freeEdgeIndices
    freeNodeIndicesOf_ = freeNodeIndices
    allNodes_          = allNodes
    allEdges_          = allEdges
    work_              = work

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

work :: [Operation] -> Graph -> Maybe Graph
work ops g = maybeRecursion ops (handleOperation) g

handleOperation :: Operation -> Graph -> Maybe Graph
handleOperation op g
  | (InsertE edges) <- op = insertEdges edges g
  | (RemoveE edges) <- op = removeEdges edges g
  | (InsertN nodes) <- op = insertNodes nodes g
  | (RemoveN nodes) <- op = removeNodes nodes g
  | (SubEdge xs) <- op = subEdges xs g

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

removeEdge :: Edge -> Graph -> Maybe Graph
removeEdge (Edge eIDX _ _) (Graph min mie)
  = Just $ Graph min (Map.delete eIDX mie)

removeNode :: Node -> Graph -> Maybe Graph
removeNode (Node nIDX _) (Graph min mie)
  = Just $ Graph (Map.delete nIDX min) mie

subEdges xs g = maybeRecursion xs (subEdge) g

subEdge :: (Node, Edge, Edge) -> Graph -> Maybe Graph
subEdge (node, target, replacement) (Graph min mie)
  | (Node nIDX _) <- node
  , (Edge targetEdgeIDX _ _) <- target
  , (Edge replaceEdgeIDX _ _) <- target
  = do
      (NodeP eIDXs) <- Map.lookup nIDX min
      eIDXs' <- Just $ [
        if i /= targetEdgeIDX then i else replaceEdgeIDX | i <- eIDXs]
      min' <- Just $ Map.insert nIDX (NodeP eIDXs') min
      return $ Graph min' mie

maybeRecursion :: [a] -> (a -> obj -> Maybe obj) -> obj -> Maybe obj
maybeRecursion xs f b
  | length xs == 0 = Just b
  | otherwise = b' >>= maybeRecursion otherX (f)
  where
    (x:otherX) = xs
    b' = return b >>= f x

removeNodes :: [Node] -> Graph -> Maybe Graph
removeNodes ns g = maybeRecursion ns (removeNode) g

removeEdges :: [Edge] -> Graph -> Maybe Graph
removeEdges es g = maybeRecursion es (removeEdge) g

insertNodes :: [Node] -> Graph -> Maybe Graph
insertNodes ns g = maybeRecursion ns (insertNode) g

insertEdges :: [Edge] -> Graph -> Maybe Graph
insertEdges es g = maybeRecursion es (insertEdge) g

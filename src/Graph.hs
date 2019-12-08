module Graph where
import Data.Map.Strict as Map hiding (take, filter)
import Data.Maybe
import FlatGraph
type Eidx = Label
type Nidx = Label
data NodeP = NodeP [Eidx] deriving (Show)
data EdgeP = EdgeP [Nidx] EdgeType deriving (Show)
data Graph = Graph (Map Nidx NodeP) (Map Eidx EdgeP) deriving (Show)

emptyGraph :: Graph
emptyGraph = Graph (Map.empty) (Map.empty)

instance FlatGraph Graph where
    freeEdgeLabelsOf_  = freeEdgeIndices
    freeNodeLabelsOf_  = freeNodeIndices
    getNode_           = getNode
    getEdge_           = getEdge
    allNodes_          = allNodes
    allEdges_          = allEdges
    split_             = Graph.split
    work_              = work

freeEdgeIndices :: Int -> Graph -> [Eidx]
freeEdgeIndices n (Graph _ me)
  | size me == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax me

freeNodeIndices :: Int -> Graph -> [Eidx]
freeNodeIndices n (Graph mn _)
  | size mn == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax mn

getNode :: Nidx -> Graph -> Maybe Node
getNode nIDX (Graph mn me) = do
  (NodeP eIDXs) <- Map.lookup nIDX mn
  return $ Node nIDX (catMaybes [ getEdge e (Graph mn me) | e <- eIDXs ])

getEdge :: Eidx -> Graph -> Maybe Edge
getEdge eIDX (Graph mn me) = do
  (EdgeP nIDXs eType) <- Map.lookup eIDX me
  return $ Edge eIDX (catMaybes [ getNode n (Graph mn me) | n <- nIDXs]) eType

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


split :: Node -> Graph -> Maybe ([Node], Graph)
split node g
  | (Node _ edges) <- node
  = pulls [ (e, node) | e <- edges ] ([], g)
  | otherwise = Nothing

work :: [Operation] -> Graph -> Maybe Graph
work ops g = maybeRecursion ops (handleOperation) g

-- UTILS --


-- TODO edgetype check the merge
merge :: (Node, Node) -> Graph -> Maybe Graph
merge (ni, nj) g
  | Node niIDX [eix] <- ni
  , Node njIDX [ejx] <- nj
  , Edge eixIDX _ _ <- eix
  , Edge ejxIDX _ _ <- ejx
  = do
      nI <- Just $ head $ 1 `freeNodeIndices` g
      nc <- Just $ Node nI [eix, ejx]
      return g >>= work_ [InsertN [nc]] >>=
        swapNidxInEidx niIDX nI eixIDX >>=
        swapNidxInEidx njIDX nI ejxIDX

{-
(ni)      __eij__      (nj) : input
(ni) (n1) __eij__ (n2) (nj) : pulls
(ni)                   (nj) : delete edge and nodes
-}
removeEdge :: Edge -> Graph -> Maybe Graph
removeEdge eij g
  | (Edge _ [ni, nj] _) <- eij
  , Just ([n1, n2], g') <- pulls [(eij, ni), (eij, nj)] ([], g)
  = return g' >>=
    deleteEdges [eij] >>= deleteNodes [n1, n2]

{-
... (ni)__edge__ ...      : input
... (ni) (nj)__edge__ ... : pull edge from node
note: (nj) is created
-}
pull :: (Edge, Node) -> ([Node], Graph) -> Maybe ([Node], Graph)
pull (edge, node) (nodes, g)
  | (Edge eidx _ _) <- edge
  , (Node nidx _) <- node
  = do
    nidx' <- Just $ head $ 1 `freeNodeLabelsOf_` g
    g' <- return g >>=
      filterOutEidxFromNidx eidx nidx  >>=
      maybeInsertNode (Node nidx' [edge]) >>=
      swapNidxInEidx nidx nidx' eidx
    return (nodes ++ [(Node nidx' [edge])], g')
  | otherwise = Nothing

handleOperation :: Operation -> Graph -> Maybe Graph
handleOperation op g
  | InsertE edges <- op = insertEdges edges g
  | InsertN nodes <- op = insertNodes nodes g
  | RemoveE edges <- op = removeEdges edges g
  | Swap nns <- op = swaps nns g
  | Merge nijs <- op = merges nijs g

getEdgeIDXs :: [Edge] -> [Eidx]
getEdgeIDXs edges = [ eIDX | Edge eIDX _ _ <- edges ]

getEdgeIDXsIn :: Node -> [Eidx]
getEdgeIDXsIn (Node _ edges) = getEdgeIDXs edges

swaps :: [(Node, Node)] -> Graph -> Maybe Graph
swaps nns graph = maybeRecursion nns (swap) graph

swap :: (Node, Node) -> Graph -> Maybe Graph
swap (target, replacement) graph
  | (Node ntIDX e0) <- target
  , (Node nrIDX e1) <- replacement
  , length e0 < length e1
  , ntIDX == nrIDX = Just $ insertNode replacement graph
  | otherwise = Nothing

insertEdge :: Edge -> Graph -> Graph
insertEdge edge graph
  | (Edge eIDX [Node n1IDX _, Node n2IDX _] eType) <- edge
  , (Graph mapNode mapEdge) <- graph
  = Graph mapNode (Map.insert eIDX (EdgeP [n1IDX, n2IDX] eType) mapEdge)

maybeInsertEdge :: Edge -> Graph -> Maybe Graph
maybeInsertEdge edge graph
  | (Edge eIDX [Node _ _, Node _ _] _) <- edge
  , (Graph _ mapEdge) <- graph
  , not $ member eIDX mapEdge = Just $ insertEdge edge graph
  | otherwise = Nothing

insertNode :: Node -> Graph -> Graph
insertNode (Node nIDX edges) (Graph mapNode mapEdge)
  = Graph (Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode) mapEdge

maybeInsertNode :: Node -> Graph -> Maybe Graph
maybeInsertNode (Node nIDX edges) (Graph mapNode mapEdge)
  | not $ member nIDX mapNode = Just graph'
  | otherwise = Nothing
  where
    mapNode' = Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode
    graph' = Graph mapNode' mapEdge

-- not a valid graph operations unless handled correctly
-- see removeEdge for the correct handling
deleteEdge :: Edge -> Graph -> Maybe Graph
deleteEdge (Edge eIDX _ _) (Graph mn me)
  = Just $ Graph mn (Map.delete eIDX me)
deleteNode :: Node -> Graph -> Maybe Graph
deleteNode (Node nIDX _) (Graph mn me)
  = Just $ Graph (Map.delete nIDX mn) me

-- RECURSION BOILERPLATE --

maybeRecursion :: [a] -> (a -> obj -> Maybe obj) -> obj -> Maybe obj
maybeRecursion xs f b
  | length xs == 0 = Just b
  | otherwise = b' >>= maybeRecursion otherX (f)
  where
    (x:otherX) = xs
    b' = return b >>= f x

pulls :: [(Edge, Node)] -> ([Node], Graph) -> Maybe ([Node], Graph)
pulls ens nsg = maybeRecursion ens (pull) nsg

deleteNodes :: [Node] -> Graph -> Maybe Graph
deleteNodes ns g = maybeRecursion ns (deleteNode) g

deleteEdges :: [Edge] -> Graph -> Maybe Graph
deleteEdges es g = maybeRecursion es (deleteEdge) g

removeEdges :: [Edge] -> Graph -> Maybe Graph
removeEdges es g = maybeRecursion es (removeEdge) g

insertNodes :: [Node] -> Graph -> Maybe Graph
insertNodes ns g = maybeRecursion ns (maybeInsertNode) g

insertEdges :: [Edge] -> Graph -> Maybe Graph
insertEdges es g = maybeRecursion es (maybeInsertEdge) g

merges :: [(Node, Node)] -> Graph -> Maybe Graph
merges nijs g = maybeRecursion nijs (merge) g

-- IDX UTILS --

filterOutEdge :: Eidx -> NodeP -> NodeP
filterOutEdge target (NodeP idxs) = NodeP $ filter
  (\e -> e/=target) idxs
{-
(n nidx) [..., eidx, ...] =>
(n nidx) [..., ...]
-}
filterOutEidxFromNidx :: Nidx -> Eidx -> Graph -> Maybe Graph
filterOutEidxFromNidx eidx nidx (Graph mn me)
  | Just (NodeP eIDXs) <- Map.lookup nidx mn
  , NodeP eIDXs' <- filterOutEdge eidx (NodeP eIDXs)
  , length eIDXs == 1 + length eIDXs'
  = if length eIDXs' == 0 then
      Just $ Graph (Map.delete nidx mn) me
    else
      Just $ Graph (Map.insert nidx (NodeP eIDXs') mn) me
  | otherwise = Nothing

swapOutNode :: Nidx -> Nidx -> EdgeP -> EdgeP
swapOutNode target replacement (EdgeP idxs eType) = EdgeP [
  if nidx==target then replacement else nidx | nidx <- idxs] eType
{-
(e eidx) [..., nidx, ...] =>
(e eidx) [..., nidx', ...]
-}
swapNidxInEidx ::  Nidx -> Nidx -> Eidx -> Graph -> Maybe Graph
swapNidxInEidx nidx nidx' eidx (Graph mn me)
  | Just (EdgeP nIDXs eType) <- Map.lookup eidx me
  , EdgeP nIDXs' _ <- swapOutNode nidx nidx' (EdgeP nIDXs eType)
  , length nIDXs == length nIDXs' && nIDXs /= nIDXs'
  = Just $ Graph mn (Map.insert eidx (EdgeP nIDXs' eType) me)
  | otherwise = Nothing

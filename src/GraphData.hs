module GraphData (
  GraphData (..), emptyGraph ) where
import Data.Map.Strict as Map hiding (take, filter)
import Data.Maybe
import GraphRecursive
type Eidx = Label
type Nidx = Label
data NodeP = NodeP [Eidx] deriving (Show, Eq)
data EdgeP = EdgeP [Nidx] EdgeType deriving (Show, Eq)
data GraphData = GraphData (Map Nidx NodeP) (Map Eidx EdgeP) deriving (Show, Eq)

emptyGraph :: GraphData
emptyGraph = GraphData (Map.empty) (Map.empty)

instance GraphRecursive GraphData where
    freeEdgeLabelsOf  = freeEdgeIndices'
    freeNodeLabelsOf  = freeNodeIndices'
    getNode           = getNode'
    getEdge           = getEdge'
    allNodes          = allNodes'
    allEdges          = allEdges'
    safeSplit         = safeSplit'
    work              = work'
    isEmpty           = isEmpty'
    swapChain         = swapChain'

isEmpty' :: GraphData -> Bool
isEmpty' (GraphData a b) = a == empty && b == empty

freeEdgeIndices' :: Int -> GraphData -> [Eidx]
freeEdgeIndices' n (GraphData _ me)
  | size me == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax me

freeNodeIndices' :: Int -> GraphData -> [Eidx]
freeNodeIndices' n (GraphData mn _)
  | size mn == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax mn

getNode' :: Nidx -> GraphData -> Maybe Node
getNode' nIDX (GraphData mn me) = do
  (NodeP eIDXs) <- Map.lookup nIDX mn
  return $ Node nIDX (catMaybes [ getEdge e (GraphData mn me) | e <- eIDXs ])

getEdge' :: Eidx -> GraphData -> Maybe Edge
getEdge' eIDX (GraphData mn me) = do
  (EdgeP nIDXs eType) <- Map.lookup eIDX me
  return $ Edge eIDX (catMaybes [ getNode n (GraphData mn me) | n <- nIDXs]) eType

allNodes' :: GraphData -> [Node]
allNodes' (GraphData mapNode mapEdge) = catMaybes [
  getNode nIDX graph | nIDX <- keys mapNode ]
  where
    graph = (GraphData mapNode mapEdge)

allEdges' :: GraphData -> [Edge]
allEdges' (GraphData mapNode mapEdge) = catMaybes [
  getEdge eIDX graph | eIDX <- keys mapEdge ]
  where
    graph = (GraphData mapNode mapEdge)


-- from a stale node and graph, gets synced node and does the split
safeSplit' :: Node -> GraphData -> Maybe ([Node], GraphData)
safeSplit' staleNode g
  | (Node nIDX _) <- staleNode
  = do
      freshNode <- getNode nIDX g
      GraphData.split freshNode g
  | otherwise = Nothing

-- assumes that the node and graph are in sync
split :: Node -> GraphData -> Maybe ([Node], GraphData)
split node g
  | (Node _ edges) <- node
  = pulls [ (e, node) | e <- edges ] ([], g)
  | otherwise = Nothing

work' :: [Operation] -> GraphData -> Maybe GraphData
work' ops g = maybeRecursion ops (handleOperation) g

-- UTILS --


-- TODO edgetype check the merge
-- merges two vectors of type
-- ... (ni) _etype_ (nj)   (nk) _etype (nl) ...
-- to
-- ... (ni) _etype_ (n') _etype_ (nl)
merge :: (Node, Node) -> GraphData -> Maybe GraphData
merge (nj, nk) g
  | Node njIDX [eij] <- nj
  , Node nkIDX [ekl] <- nk
  , (Edge ejiIDX [_, ni] ejiType) <- orientEdge nj eij
  , (Edge eklIDX [_, nl] eklType) <- orientEdge nk ekl
  , ejiType == invert eklType
  = do
      nidx' <- Just $ head $ 1 `freeNodeIndices'` g
      n' <- Just $ Node nidx' [eij, ekl]
      --eji' = Edge ejiIDX [n', nk] ejiType
      --ekl' = Edge eklIDX [n', nj] eklType
      g' <- Just $  insertNode n' g -- $ insertEdge eji'  $ insertEdge ekl' g
      return g' >>=
        swapNidxInEidx nkIDX nidx' eklIDX >>=
        swapNidxInEidx njIDX nidx' ejiIDX >>=
        deleteNode nj >>= deleteNode nk

-- >>= deleteNode nk

{-
(ni)      __eij__      (nj) : input
(ni) (n1) __eij__ (n2) (nj) : pulls
(ni)                   (nj) : delete edge and nodes
-}
removeEdge :: Edge -> GraphData -> Maybe GraphData
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
pull :: (Edge, Node) -> ([Node], GraphData) -> Maybe ([Node], GraphData)
pull (edge, node) (nodes, g)
  | (Edge eidx _ _) <- edge
  , (Node nidx _) <- node
  = do
    nidx' <- Just $ head $ 1 `freeNodeIndices'` g
    g' <- return g >>=
      filterOutEidxFromNidx eidx nidx  >>=
      maybeInsertNode (Node nidx' [edge]) >>=
      swapNidxInEidx nidx nidx' eidx
    n' <- getNode' nidx' g'
    return (nodes ++ [n'], g')
  | otherwise = Nothing

handleOperation :: Operation -> GraphData -> Maybe GraphData
handleOperation op g
  | InsertE edges <- op = insertEdges edges g
  | InsertN nodes <- op = insertNodes nodes g
  | RemoveE edges <- op = removeEdges edges g
  | Swap nns <- op = swaps nns g
  | Merge nijs <- op = merges nijs g
  | DeleteN nodes <- op = deleteNodes nodes g
  | DeleteE edges <- op = deleteEdges edges g

getEdgeIDXs :: [Edge] -> [Eidx]
getEdgeIDXs edges = [ eIDX | Edge eIDX _ _ <- edges ]

getEdgeIDXsIn :: Node -> [Eidx]
getEdgeIDXsIn (Node _ edges) = getEdgeIDXs edges

swaps :: [(Node, Node)] -> GraphData -> Maybe GraphData
swaps nns graph = maybeRecursion nns (swap) graph

-- TODO verify label equality
swap :: (Node, Node) -> GraphData -> Maybe GraphData
swap (target, replacement) graph
  | (Node ntIDX e0) <- target
  , (Node nrIDX e1) <- replacement
-- , length e0 == length e1
  , ntIDX == nrIDX = Just $ insertNode replacement graph
  | otherwise = Nothing

insertEdge :: Edge -> GraphData -> GraphData
insertEdge edge graph
  | (Edge eIDX [Node n1IDX _, Node n2IDX _] eType) <- edge
  , (GraphData mapNode mapEdge) <- graph
  = GraphData mapNode (Map.insert eIDX (EdgeP [n1IDX, n2IDX] eType) mapEdge)

maybeInsertEdge :: Edge -> GraphData -> Maybe GraphData
maybeInsertEdge edge graph
  | (Edge eIDX [Node _ _, Node _ _] _) <- edge
  , (GraphData _ mapEdge) <- graph
  , not $ member eIDX mapEdge = Just $ insertEdge edge graph
  | otherwise = Nothing

insertNode :: Node -> GraphData -> GraphData
insertNode (Node nIDX edges) (GraphData mapNode mapEdge)
  = GraphData (Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode) mapEdge

maybeInsertNode :: Node -> GraphData -> Maybe GraphData
maybeInsertNode (Node nIDX edges) (GraphData mapNode mapEdge)
  | not $ member nIDX mapNode = Just graph'
  | otherwise = Nothing
  where
    mapNode' = Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode
    graph' = GraphData mapNode' mapEdge

-- not a valid graph operations unless handled correctly
-- see removeEdge for the correct handling
deleteEdge :: Edge -> GraphData -> Maybe GraphData
deleteEdge (Edge eIDX _ _) (GraphData mn me)
  = Just $ GraphData mn (Map.delete eIDX me)
deleteNode :: Node -> GraphData -> Maybe GraphData
deleteNode (Node nIDX _) (GraphData mn me)
  = Just $ GraphData (Map.delete nIDX mn) me

-- RECURSION BOILERPLATE --

maybeRecursion :: [a] -> (a -> obj -> Maybe obj) -> obj -> Maybe obj
maybeRecursion xs f b
  | length xs == 0 = Just b
  | otherwise = b' >>= maybeRecursion otherX (f)
  where
    (x:otherX) = xs
    b' = return b >>= f x

pulls :: [(Edge, Node)] -> ([Node], GraphData) -> Maybe ([Node], GraphData)
pulls ens nsg = maybeRecursion ens (pull) nsg

deleteNodes :: [Node] -> GraphData -> Maybe GraphData
deleteNodes ns g = maybeRecursion ns (deleteNode) g

deleteEdges :: [Edge] -> GraphData -> Maybe GraphData
deleteEdges es g = maybeRecursion es (deleteEdge) g

removeEdges :: [Edge] -> GraphData -> Maybe GraphData
removeEdges es g = maybeRecursion es (removeEdge) g

insertNodes :: [Node] -> GraphData -> Maybe GraphData
insertNodes ns g = maybeRecursion ns (maybeInsertNode) g

insertEdges :: [Edge] -> GraphData -> Maybe GraphData
insertEdges es g = maybeRecursion es (maybeInsertEdge) g

merges :: [(Node, Node)] -> GraphData -> Maybe GraphData
merges nijs g = maybeRecursion nijs (merge) g

-- IDX UTILS --

filterOutEdge :: Eidx -> NodeP -> NodeP
filterOutEdge target (NodeP idxs) = NodeP $ filter
  (\e -> e/=target) idxs
{-
(n nidx) [..., eidx, ...] =>
(n nidx) [..., ...]
-}
filterOutEidxFromNidx :: Nidx -> Eidx -> GraphData -> Maybe GraphData
filterOutEidxFromNidx eidx nidx (GraphData mn me)
  | Just (NodeP eIDXs) <- Map.lookup nidx mn
  , NodeP eIDXs' <- filterOutEdge eidx (NodeP eIDXs)
  , length eIDXs == 1 + length eIDXs'
  = if length eIDXs' == 0 then
      Just $ GraphData (Map.delete nidx mn) me
    else
      Just $ GraphData (Map.insert nidx (NodeP eIDXs') mn) me
  | otherwise = Nothing

swapOutNode :: Nidx -> Nidx -> EdgeP -> EdgeP
swapOutNode target replacement (EdgeP idxs eType) = EdgeP [
  if nidx==target then replacement else nidx | nidx <- idxs] eType

swapOutEdge :: Eidx -> Eidx -> NodeP -> NodeP
swapOutEdge target replacement (NodeP idxs) = NodeP [
  if eidx==target then replacement else eidx | eidx <- idxs]


{-
(e eidx) [..., nidx, ...] =>
(e eidx) [..., nidx', ...]
-}
swapNidxInEidx ::  Nidx -> Nidx -> Eidx -> GraphData -> Maybe GraphData
swapNidxInEidx nidx nidx' eidx (GraphData mn me)
  | Just (EdgeP nIDXs eType) <- Map.lookup eidx me
  , EdgeP nIDXs' _ <- swapOutNode nidx nidx' (EdgeP nIDXs eType)
  , length nIDXs == length nIDXs' && nIDXs /= nIDXs'
  = Just $ GraphData mn (Map.insert eidx (EdgeP nIDXs' eType) me)
  | otherwise = Nothing

swapEidxInNidx :: Eidx -> Eidx -> Nidx -> GraphData -> Maybe GraphData
swapEidxInNidx eidx eidx' nidx (GraphData mn me)
  | Just (NodeP eIDXs) <- Map.lookup nidx mn
  , NodeP eIDXs' <- swapOutEdge eidx eidx' (NodeP eIDXs)
  , length eIDXs == length eIDXs' && eIDXs /= eIDXs'
  = Just $ GraphData (Map.insert nidx (NodeP eIDXs') mn) me
  | otherwise = Nothing

swapEIDX :: (Nidx, Eidx) -> Eidx -> GraphData -> Maybe GraphData
swapEIDX (nidx, eidx) eidx' (GraphData mn me) =
  swapEidxInNidx eidx eidx' nidx (GraphData mn me)

swapIn :: (GraphData, Node, Edge) -> Edge -> Maybe GraphData
swapIn (g, n, e) e'
  | (Edge eIDX _ _) <- e
  , (Edge eIDX' _ _) <- e'
  , (Node nIDX _) <- n
  = swapEidxInNidx eIDX eIDX' nIDX g

-- TODO could be generalized to chains of any length
swapChain' :: (Node, Edge, Node, Edge, Node) -> Edge -> GraphData -> Maybe GraphData
swapChain' (ni, eij, nj, ejk, nk) eik g
  = return g >>=
    swapEIDX (nidx ni, eidx eij) (eidx eik) >>=
    swapEIDX (nidx nk, eidx ejk) (eidx eik) >>=
    deleteNode nj >>= deleteEdge eij >>= deleteEdge ejk >>=
    maybeInsertEdge eik

eidx :: Edge -> Eidx
eidx (Edge idx _ _ ) = idx

nidx :: Node -> Nidx
nidx (Node idx _) = idx



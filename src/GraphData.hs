module GraphData (
 GraphData (..), emptyGraph ) where
import Data.Map.Strict as Map hiding (take, filter, map, splitAt)
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Foldable as Foldable
import GraphRecursive
import Prelude hiding (product)
type Eidx = Label
type Nidx = Label
data NodeP = NodeP [Eidx] deriving (Show, Eq)
data EdgeP = EdgeP [Nidx] EdgeType deriving (Show, Eq)
data GraphData = GraphData (Map Nidx NodeP) (Map Eidx EdgeP) deriving (Show, Eq)

emptyGraph :: GraphData
emptyGraph = GraphData (Map.empty) (Map.empty)

instance GraphRecursive GraphData where
    splitNode  = splitNode'
    mergeNodes = mergeNodes'
    product    = product'
    removeNode = removeNode'
    removeEdge = removeEdge'

    freeEdgeLabelsOf  = freeEdgeLabels'
    freeNodeLabelsOf  = freeNodeLabels'
    getNode           = getNode'
    getEdge           = getEdge'
    allNodes          = allNodes'
    allEdges          = allEdges'
    isEmpty           = isEmpty'


-- NOTE top level typeclass functions


-- from a stale node and graph, gets synced node and does the split
splitNode' :: Node -> GraphData -> Maybe ([Node], GraphData)
splitNode' staleNode g
  | N nIDX _ <- staleNode
  = do
      freshNode <- getNode nIDX g
      GraphData.split freshNode g
  | otherwise = Nothing


mergeNodes' :: [Node] -> GraphData -> Maybe (Node, GraphData)
mergeNodes' nodes g
  | edges <- catMaybes $ map oneEdge nodes
  , length nodes == length edges
  = do
      nidx' <- return $ head $ 1 `freeNodeLabels'` g
      nodeLabels <- return [ l | (N l _) <- nodes]
      edgeLabels <- return [ l | (E l _ _) <- edges]
      infos <- return $ zip3 nodeLabels (repeat nidx') edgeLabels
      g' <- return g >>=
        insertNodes [N nidx' edges] >>=
        swapNwithNinE infos >>=
        deleteNodes nodes
      n' <- getNode nidx' g'
      return (n', g')

product' :: ([Node], [Edge]) -> GraphData ->  Maybe GraphData
product' (nodes, edges) g
  | length ghostNodes == length ghostPairs
  = return g >>=
    updateNodes mergedNodes >>=
    insertNodes otherNodes >>=
    insertEdges edges
  where
    ghostNodes = filter hasGhost nodes
    ghostLabels = [ l | (N l _) <- ghostNodes]
    otherNodes = [ (N l es ) | (N l es ) <- nodes,
                   not (elem l ghostLabels)]

    ghostBases = [ getNode l g | (N l _) <- ghostNodes]
    ghostPairs = zip (catMaybes ghostBases) ghostNodes
    mergedNodes = map (\(a,b) -> clickNodes a b) ghostPairs

--(ni) -> (nj) -> (nk)
--(ni) -> (nk)
removeNode' :: Node -> GraphData -> Maybe (Edge, GraphData)
removeNode' nj g
  | (N njLabel [eji, ejk]) <- oriented nj
  , not (eji =@ ejk)
  , (rotate eji) =~ ejk
  , eikLabel <- head $ 1 `freeEdgeLabels'` g
  = do
      ni <- otherNode eji nj
      nk <- otherNode ejk nj
      (N niLabel _) <- return ni
      (N nkLabel _) <- return nk
      eik <- return $ E eikLabel [ni, nk] (edgeType ejk)
      g' <- return g >>=
        swapEwithEinN [ (eidx eji, eikLabel, niLabel),
                        (eidx ejk, eikLabel, nkLabel) ] >>=
        product' ([],[eik]) >>=
        deleteNodes [nj] >>=
        deleteEdges [eji, ejk]
      return (eik, g')

removeEdge' :: Edge -> GraphData -> Maybe ((Node, Node), GraphData)
removeEdge' eij g
  | (E _ [ni, nj] _) <- eij
  , Just ([n1, n2], g') <- pulls [(eij, ni), (eij, nj)] ([], g)
  = do
      g'' <- return g' >>=
             deleteEdges [eij] >>=
             deleteNodes [n1, n2]
      ni' <- getNode' (nidx ni) g''
      nj' <- getNode' (nidx nj) g''
      return ((ni', nj'), g'')

freeEdgeLabels' :: Int -> GraphData -> [Eidx]
freeEdgeLabels' n (GraphData _ me)
  | size me == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax me

freeNodeLabels' :: Int -> GraphData -> [Eidx]
freeNodeLabels' n (GraphData mn _)
  | size mn == 0 = take n [0..]
  | otherwise = take n [(k+1)..]
  where
    (k, _) = Map.findMax mn

getNode' :: Nidx -> GraphData -> Maybe Node
getNode' nIDX (GraphData mn me) = do
  (NodeP eIDXs) <- Map.lookup nIDX mn
  return $ N nIDX (catMaybes [ getEdge e (GraphData mn me) | e <- eIDXs ])

getEdge' :: Eidx -> GraphData -> Maybe Edge
getEdge' eIDX (GraphData mn me) = do
  (EdgeP nIDXs eType) <- Map.lookup eIDX me
  return $ E eIDX (catMaybes [ getNode n (GraphData mn me) | n <- nIDXs]) eType

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

isEmpty' :: GraphData -> Bool
isEmpty' (GraphData a b) = a == empty && b == empty

-- NOTE helper functions NOTE

-- combine two nodes together on ghost edges
clickNodes :: Node -> Node -> Node
clickNodes (N a baseEdges) (N b partialEdges)
  | a == b
  , [e1] <- baseEdges
  , [idx] <- ghostidxs
  = N a (Foldable.toList $
         Seq.update idx e1 edgeSeq)
  | a == b
  , [e1,e2] <- baseEdges
  , [idx0, idx1] <- ghostidxs
  = N a (Foldable.toList $
         Seq.update idx1 e2 $
         Seq.update idx0 e1 edgeSeq)
  where
    edgeSeq  = Seq.fromList partialEdges
    ghostidxs = [ i | (i, e) <- zip [0..] partialEdges, e =~ Ghost]

removeEdgeBasic :: Edge -> GraphData -> Maybe GraphData
removeEdgeBasic eij g
  | (E _ [ni, nj] _) <- eij
  , Just ([n1, n2], g') <- pulls [(eij, ni), (eij, nj)] ([], g)
  = do
      g'' <- return g' >>=
             deleteEdges [eij] >>=
             deleteNodes [n1, n2]
      return g''

-- assumes that the node and graph are in sync
split :: Node -> GraphData -> Maybe ([Node], GraphData)
split node g
  | (N _ edges) <- node
  = pulls [ (e, node) | e <- edges ] ([], g)
  | otherwise = Nothing


-- UTILS --


-- TODO edgetype check the merge
-- TODO note that type comparison is not made so you can have metric tensors
-- merges two vectors of type
-- ... (ni) _etype_ (nj)   (nk) _etype (nl) ...
-- to
-- ... (ni) _etype_ (n') _etype_ (nl)
merge :: (Node, Node) -> GraphData -> Maybe GraphData
merge (nj, nk) g
  | N njIDX [eij] <- nj
  , N nkIDX [ekl] <- nk
  , (E ejiIDX [_, ni] ejiType) <- orientEdge nj eij
  , (E eklIDX [_, nl] eklType) <- orientEdge nk ekl
--  , ejiType == invert eklType
  = do
      nidx' <- Just $ head $ 1 `freeNodeLabels'` g
      n' <- Just $ N nidx' [eij, ekl]
      --eji' = Edge ejiIDX [n', nk] ejiType
      --ekl' = Edge eklIDX [n', nj] eklType
      g' <- Just $  insertNode n' g -- $ insertEdge eji'  $ insertEdge ekl' g
      return g' >>=
        swapNidxInEidx (nkIDX, nidx', eklIDX) >>=
        swapNidxInEidx (njIDX, nidx', ejiIDX) >>=
        deleteNode nj >>= deleteNode nk


{-
... (ni)__edge__ ...      : input
... (ni) (nj)__edge__ ... : pull edge from node
note: (nj) is created
-}
pull :: (Edge, Node) -> ([Node], GraphData) -> Maybe ([Node], GraphData)
pull (edge, node) (nodes, g)
  | (E eidx _ _) <- edge
  , (N nidx _) <- node
  = do
    nidx' <- Just $ head $ 1 `freeNodeLabels'` g
    g' <- return g >>=
      filterOutEidxFromNidx eidx nidx  >>=
      maybeInsertNode (N nidx' [edge]) >>=
      swapNidxInEidx (nidx, nidx', eidx)
    n' <- getNode' nidx' g'
    return (nodes ++ [n'], g')
  | otherwise = Nothing


updateEdgeTypes :: [(Edge, EdgeType)] -> GraphData -> Maybe GraphData
updateEdgeTypes infos graph = maybeRecursion infos (updateEdgeType) graph

--GraphData (Map Nidx NodeP) (Map Eidx EdgeP)
updateEdgeType :: (Edge, EdgeType) -> GraphData -> Maybe GraphData
updateEdgeType ((E eL ns _), edgeType') g
  | (GraphData _ mapEdge) <- g
  , member eL mapEdge
  = Just $ insertEdge (E eL ns edgeType') g
  | otherwise = Nothing

getEdgeIDXs :: [Edge] -> [Eidx]
getEdgeIDXs edges = [ eIDX | E eIDX _ _ <- edges ]

getEdgeIDXsIn :: Node -> [Eidx]
getEdgeIDXsIn (N _ edges) = getEdgeIDXs edges

{-
swaps :: [(Node, Node)] -> GraphData -> Maybe GraphData
swaps nns graph = maybeRecursion nns (swap) graph
-}

-- TODO verify label equality
swap :: (Node, Node) -> GraphData -> Maybe GraphData
swap (target, replacement) graph
  | (N ntIDX e0) <- target
  , (N nrIDX e1) <- replacement
-- , length e0 == length e1
  , ntIDX == nrIDX = Just $ insertNode replacement graph
  | otherwise = Nothing

-- naive overwrite method
insertEdge :: Edge -> GraphData -> GraphData
insertEdge edge graph
  | (E eIDX [N n1IDX _, N n2IDX _] eType) <- edge
  , (GraphData mapNode mapEdge) <- graph
  = GraphData mapNode (Map.insert eIDX (EdgeP [n1IDX, n2IDX] eType) mapEdge)

-- checks on edge membership in graph first
maybeInsertEdge :: Edge -> GraphData -> Maybe GraphData
maybeInsertEdge edge graph
  | (E eIDX [N _ _, N _ _] _) <- edge
  , (GraphData _ mapEdge) <- graph
  , not $ member eIDX mapEdge = Just $ insertEdge edge graph
  | otherwise = Nothing

insertNode :: Node -> GraphData -> GraphData
insertNode (N nIDX edges) (GraphData mapNode mapEdge)
  = GraphData (Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode) mapEdge

maybeInsertNode :: Node -> GraphData -> Maybe GraphData
maybeInsertNode (N nIDX edges) (GraphData mapNode mapEdge)
  | not $ member nIDX mapNode = Just graph'
  | otherwise = Nothing
  where
    mapNode' = Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode
    graph' = GraphData mapNode' mapEdge

maybeUpdateNode :: Node -> GraphData -> Maybe GraphData
maybeUpdateNode (N nIDX edges) (GraphData mapNode mapEdge)
  | member nIDX mapNode = Just graph'
  | otherwise = Nothing
  where
    mapNode' = Map.insert nIDX (NodeP $ getEdgeIDXs edges) mapNode
    graph' = GraphData mapNode' mapEdge

-- not a valid graph operations unless handled correctly
-- see removeEdge for the correct handling
deleteEdge :: Edge -> GraphData -> Maybe GraphData
deleteEdge (E eIDX _ _) (GraphData mn me)
  = Just $ GraphData mn (Map.delete eIDX me)
deleteNode :: Node -> GraphData -> Maybe GraphData
deleteNode (N nIDX _) (GraphData mn me)
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
removeEdges es g = maybeRecursion es (removeEdgeBasic) g

insertNodes :: [Node] -> GraphData -> Maybe GraphData
insertNodes ns g = maybeRecursion ns (maybeInsertNode) g

updateNodes :: [Node] -> GraphData -> Maybe GraphData
updateNodes ns g = maybeRecursion ns (maybeUpdateNode) g

-- old with new in
swapEwithEinN :: [(Eidx, Eidx, Nidx)] -> GraphData -> Maybe GraphData
swapEwithEinN infos g = maybeRecursion infos (swapEidxInNidx) g

-- old with new in
swapNwithNinE :: [(Nidx, Nidx, Eidx)] -> GraphData -> Maybe GraphData
swapNwithNinE infos g = maybeRecursion infos (swapNidxInEidx) g

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
TODO membership test?
(e eidx) [..., nidx, ...] =>
(e eidx) [..., nidx', ...]
-}
swapNidxInEidx ::  (Nidx, Nidx, Eidx) -> GraphData -> Maybe GraphData
swapNidxInEidx (nidx, nidx', eidx) (GraphData mn me)
  | Just (EdgeP nIDXs eType) <- Map.lookup eidx me
  , EdgeP nIDXs' _ <- swapOutNode nidx nidx' (EdgeP nIDXs eType)
  , length nIDXs == length nIDXs' && nIDXs /= nIDXs'
  = Just $ GraphData mn (Map.insert eidx (EdgeP nIDXs' eType) me)
  | otherwise = Nothing

-- old with new in
swapEidxInNidx :: (Eidx, Eidx, Nidx) -> GraphData -> Maybe GraphData
swapEidxInNidx (eidx, eidx', nidx) (GraphData mn me)
  | Just (NodeP eIDXs) <- Map.lookup nidx mn
  , NodeP eIDXs' <- swapOutEdge eidx eidx' (NodeP eIDXs)
  , length eIDXs == length eIDXs' && eIDXs /= eIDXs'
  = Just $ GraphData (Map.insert nidx (NodeP eIDXs') mn) me
  | otherwise = Nothing

swapEIDX :: (Nidx, Eidx) -> Eidx -> GraphData -> Maybe GraphData
swapEIDX (nidx, eidx) eidx' (GraphData mn me) =
  swapEidxInNidx (eidx, eidx', nidx) (GraphData mn me)

swapIn :: (GraphData, Node, Edge) -> Edge -> Maybe GraphData
swapIn (g, n, e) e'
  | (E eIDX _ _) <- e
  , (E eIDX' _ _) <- e'
  , (N nIDX _) <- n
  = swapEidxInNidx (eIDX, eIDX', nIDX) g

-- TODO could be generalized to chains of any length
swapChain' :: (Node, Edge, Node, Edge, Node) -> Edge -> GraphData -> Maybe GraphData
swapChain' (ni, eij, nj, ejk, nk) eik g
  = return g >>=
    swapEIDX (nidx ni, eidx eij) (eidx eik) >>=
    swapEIDX (nidx nk, eidx ejk) (eidx eik) >>=
    deleteNode nj >>= deleteEdge eij >>= deleteEdge ejk >>=
    maybeInsertEdge eik

eidx :: Edge -> Eidx
eidx (E idx _ _ ) = idx

nidx :: Node -> Nidx
nidx (N idx _) = idx

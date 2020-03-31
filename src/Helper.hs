module Helper (
  passGraph, before, after, vectMatch, isNodeTadpole, idxsMatch,
  cyclePermute
  ) where
import GraphRecursive

-- EXPORTS

passGraph :: (GraphRecursive g) => (a, g) -> Maybe g
passGraph (_, g) = Just g

before :: Edge -> Node -> Maybe Edge
before (E l _ _) (N _ edges)
  | Just idx <- idxsMatch l edges
  , cycleLength <- length edges
  = Just $ cycle edges !! (idx + cycleLength - 1)
  | otherwise = Nothing

after :: Edge -> Node -> Maybe Edge
after (E l _ _) (N _ edges)
  | Just idx <- idxsMatch l edges
  , cycleLength <- length edges
  = Just $ cycle edges !! (idx + cycleLength + 1)
  | otherwise = Nothing

vectMatch :: EdgeType -> [Node] -> Node
vectMatch targetType nodes
  | length fNodes == 1 = head fNodes
  where
    teq (N _ [E _ _ eType]) = eType == targetType
    fNodes = [n | n <- nodes, teq $ oriented n]

isNodeTadpole :: Node -> Bool
isNodeTadpole node
  | N _ [e1, e2, e3] <- node
  , areEdgesTadpole [e1, e2, e3] ||
    areEdgesTadpole [e3, e1, e2] ||
    areEdgesTadpole [e2, e3, e1]
  = True
  | otherwise = False


cyclePermute xs = [ take len $ drop i $ cycle xs | i <- [1..len] ]
  where
    len = length xs


-- UTILS

-- get idx (in [Node] list) where first node matches label
idxsMatch :: Label -> [Edge] -> Maybe Int
idxsMatch l edges
  | length idxs >= 1 = Just $ head idxs
  | otherwise = Nothing
  where
    idxs = [ i | (i, E l' _ _) <- zip [0..] edges, l == l']

areEdgesTadpole :: [Edge] -> Bool
areEdgesTadpole [e1, e2, e3]
  | e1 =@ e2
  , e1 `isEdgeType` U || e2 `isEdgeType` D
  , e3 `isEdgeType` G
  = True
  | otherwise = False

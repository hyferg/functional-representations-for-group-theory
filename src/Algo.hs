module Algo (build, buildNode, foldNode) where
import Rules
import GraphData
import MathObj.LaurentPolynomial as LP
import Data.Maybe

data Strat g = EdgeStrat (
  (Scope g -> Maybe (Decomposed g)),
  (String),
  (g -> [Edge])) |
               NodeStrat (
  (Scope g -> Maybe (Decomposed g)),
  (String),
  (g -> [Node]))


strategy :: (GraphRecursive g) => [Strat g]
strategy = [ EdgeStrat (sunP1Rule, "sunp1", (gluonEdges . allEdges)),
             NodeStrat (loopRule, "loop", allNodes),
             NodeStrat (shrinkChainRule, "chain", allNodes),
             NodeStrat (tadpoleRule, "tadpole", allNodes),
             NodeStrat (gggRule, "ggg", allNodes),
             EdgeStrat (twistRule, "twist", allEdges) ]

  {-
buildNode :: (GraphRecursive g) =>
             VectorSpace Poly g ->
             ((Poly, DebugInfo), [VectorSpace Poly g])
buildNode preNode
  | Left  ((poly', vs), id) <- stack = ((poly', id, Nothing), vs)
  | Right (VS poly' g) <- stack = ((poly', "fail", Just $ g), [])
  where
    stack = return preNode >>= (decompose strategy)


-}

type TreeVertex g = ((Poly, String, Maybe g), [VectorSpace g])

buildNode :: (GraphRecursive g) => VectorSpace g -> TreeVertex g
buildNode vs
  | (VS p g) <- vs
  , isEmpty g = ((p, "empty", Nothing), [])
  | length out >= 1 = head out
  | (VS p g) <- vs = ((p, "fail", Just g), [])
  where
    out = superDecomposition strategy vs

superDecomposition :: (GraphRecursive g) =>
  [Strat g] -> VectorSpace g -> [TreeVertex g]
superDecomposition strats vs = concat $ map (applyStrat vs) strats

applyStrat :: (GraphRecursive g) =>
  VectorSpace g -> Strat g -> [TreeVertex g]
applyStrat vs strat
  | NodeStrat info <- strat = f info $ buildNodeScope info vs
  | EdgeStrat info <- strat = f info $ buildEdgeScope info vs
  where
    addInfo (n, g) (p, vss) = ((p, n, g), vss)
    f (rule, name, _) = map (addInfo (name, Nothing)) . catMaybes . map rule

    buildNodeScope (_, _, elems) (VS p g) =
      [ NodeScope (n, (VS p g)) | n <- elems g ]

    buildEdgeScope (_, _, elems) (VS p g) =
      [ EdgeScope (e, (VS p g)) | e <- elems g ]



  {-
decompose :: (GraphRecursive g) => [Strat g] -> VectorSpace Poly g -> Either (Poly, [VectorSpace Poly g]) (VectorSpace Poly g)
decompose strategy (VS poly g)
  | length superDecomposition >= 1 = Left $ head superDecomposition
  | otherwise = Right (VS poly g)
  where
    superDecomposition = concat $ map (applyRule (VS poly g)) strategy
-}

build :: (GraphData -> Maybe GraphData) -> VectorSpace GraphData
build someGraphOn
  | Just g <- someGraphOn emptyGraph = VS plusOne g
  | otherwise = VS zero emptyGraph

gluonEdges :: [Edge] -> [Edge]
gluonEdges edges = filter (\x -> x `is` G) edges

foldNode :: (GraphRecursive g) => (Poly, [Char], Maybe g) -> [Poly] -> Poly
foldNode (a, _, Nothing) [b, c] = LP.mul a (LP.add b c)
foldNode (a, _, Nothing) [b] = LP.mul a b
foldNode (a, _, Nothing) [] = a

zero :: Poly
zero = fromCoeffs []

plusOne :: Poly
plusOne = fromCoeffs [1]


-- poly' == poly as a rule in general
-- however one could imagine taking the input poly coeff and distributing it
-- over the child nodes so there could potentially be an exception
{-

-}


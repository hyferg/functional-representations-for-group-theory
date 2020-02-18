module Algo (build, buildNode, foldNode) where
import Rules
import Poly
import GraphData
import Data.Maybe

type Rule g = (Scope g -> Maybe (Decomposed g))
data Strat g = EdgeStrat (Rule g, String, (g -> [Edge])) |
               NodeStrat (Rule g, String, (g -> [Node]))

type TreeVertex g = ((Poly, String, Maybe g), [VectorSpace g])


strategy :: (GraphRecursive g) => [Strat g]
strategy = [ EdgeStrat (sunP1Rule, "sunp1", (gluonEdges . allEdges)),
             NodeStrat (loopRule, "loop", allNodes),
             NodeStrat (shrinkChainRule, "chain", allNodes),
             NodeStrat (tadpoleRule, "tadpole", allNodes),
             NodeStrat (gggRule, "ggg", allNodes),
             EdgeStrat (twistRule, "twist", allEdges) ]

buildNode :: (GraphRecursive g) => VectorSpace g -> TreeVertex g
buildNode vs
  | (VS p g) <- vs, isEmpty g = ((p, "empty", Nothing), [])
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

gluonEdges :: [Edge] -> [Edge]
gluonEdges edges = filter (\x -> x `is` G) edges

foldNode :: (GraphRecursive g) => (Poly, [Char], Maybe g) -> [Poly] -> Poly
foldNode (a, _, Nothing) [b, c] = mul a (add b c)
foldNode (a, _, Nothing) [b] = mul a b
foldNode (a, _, Nothing) [] = a

build :: (GraphData -> Maybe GraphData) -> VectorSpace GraphData
build someGraphOn
  | Just g <- someGraphOn emptyGraph = VS plusOne g
  | otherwise = VS zero emptyGraph

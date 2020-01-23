import Graph
import FlatGraph
import Decompositions
import AddGraph
import MathObj.LaurentPolynomial as LP
import Data.Maybe
import Data.Tree

main :: IO ()
main = return ()

-- poly' == poly as a rule in general
-- however one could imagine taking the input poly coeff and distributing it
-- over the child nodes so there could potentially be an exception
{-

-}

rep = putStr $ drawTree $ fmap show out

foldNode :: (FlatGraph g) => (Poly, [Char], Maybe g) -> [Poly] -> Poly
foldNode (a, _, Nothing) [b, c] = LP.mul a (LP.add b c)
foldNode (a, _, Nothing) [b] = LP.mul a b
foldNode (a, _, Nothing) [] = a

--input = build pill
input = build fourCasimir
--out = buildNode input
out = unfoldTree buildNode input
poly = foldTree foldNode out

buildNode :: (FlatGraph g) =>
  VectorSpace Poly g ->
  ((Poly, [Char], Maybe g), [VectorSpace Poly g])
buildNode preNode
  | Left  ((poly', vs), id) <- stack = ((poly', id, Nothing), vs)
  | Right (VS poly' g) <- stack = ((poly', "fail", Just $ g), [])
  where
    stack = return preNode >>= decompose

-- TODO put this in a list format so different strats can easily be tried
decompose :: (FlatGraph g) =>
  VectorSpace Poly g ->
  Either ((Poly, [VectorSpace Poly g]), [Char])
                (VectorSpace Poly g)
decompose (VS poly g)

  | input <- [ (node, (VS poly g)) | node <- allNodes_ g ]
  , out <- catMaybes $ map shrinkChainRule input
  , length out >= 1 = Left $ ((head out), "chainx")

  | input <- [ (node, (VS poly g)) | node <- allNodes_ g ]
  , out <- catMaybes $ map tadpoleRule input
  , length out >= 1 = Left $ ((head out), "tadpole")

  | input <- [ (edge, (VS poly g)) | edge <- gluonEdges $ allEdges_ g ]
  , out <- catMaybes $ map sunP1Rule input
  , length out >= 1 = Left $ ((head out), "sunp1")

  | input <- [ (node, (VS poly g)) | node <- allNodes_ g ]
  , out <- catMaybes $ map loopRule input
  , length out >= 1 = Left $ ((head out), "loop")


  | isEmpty_ g = Left $ ((poly, []), "empty")

  -- | otherwise = Right $ VS poly g

{-


  | input <- [ (node, (VS poly g)) | node <- allNodes_ g ]
  , out <- catMaybes $ map gggRule input
  , length out >= 1 = Left $ head out




-}




build :: (Graph -> Maybe Graph) -> VectorSpace Poly Graph
build someGraphOn
  | Just g <- someGraphOn emptyGraph = VS plusOne g
  | otherwise = VS zero emptyGraph

gluonEdges :: [Edge] -> [Edge]
gluonEdges edges = [ e | e <- edges, e =-= (Edge 0 [] G)]


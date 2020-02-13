import Algo
import Graphs

import Data.Tree

main :: IO ()
main = return ()

input = build peace6j
out = unfoldTree buildNode input
poly = foldTree foldNode out
rep = putStr $ drawTree $ fmap show out




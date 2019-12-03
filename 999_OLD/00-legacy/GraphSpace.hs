module GraphSpace where
import EdgeSpaces

class GraphSpace g where
  construct :: (EdgeSpace poly) -> g
  couldHave :: g -> (EdgeSpace poly) -> Bool
  find :: g -> (EdgeSpace poly) -> index
  return :: g -> index -> g
  sub :: g -> index -> (EdgeSpace poly) -> g

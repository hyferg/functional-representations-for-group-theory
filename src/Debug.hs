module Debug where
import FlatGraph

--sunP1SplitDebug :: (FlatGraph g) => Edge -> g -> Maybe (g, g)
sunP1SplitDebug emn g
  | (Edge _ [nm, nn] G) <- emn
  , isClock nm && isClock nn || isAntiClock nm && isAntiClock nn
  = do
      (emi, emk) <- otherEdges (oriented nm) emn
      (enl, enj) <- otherEdges (oriented nn) emn

      ni <- otherNode emi nm
      nk <- otherNode emk nm
      nj <- otherNode enj nn
      nl <- otherNode enl nn

      let
        eI = 2 `freeEdgeIndicesOf_` g

        remove = [
          --RemoveN [nm, nn],
          RemoveE [emi, emk, enj, enl, emn] ]

        -- lhs edges (+1)
        eji = Edge (eI !! 0) [nj, ni] $ edgeType emi
        ekl = Edge (eI !! 1) [nk, nl] $ edgeType enl
        lhs = return g >>= work_ (
          remove ++ [
              InsertE [eji, ekl],
              SubEdge [(ni, emi, eji),
                       (nj, enj, eji),
                       (nk, emk, ekl),
                       (nl, enl, ekl) ]])

        in lhs


  {-

        -- rhs edges (-1/n)
        eki = Edge (eI !! 0) [nk, ni] $ edgeType emi
        ejl = Edge (eI !! 1) [nj, nl] $ edgeType enl
        rhs = return g >>= work_ (
          remove ++ [
              InsertE [eki, ejl],
              SubEdge [(ni, emi, eki),
                       (nk, emk, eki),
                       (nj, enj, ejl),
                       (nl, enl, ejl) ]])

        in do
          l <- lhs
          r <- rhs
          return (l, r)
-}
  | otherwise = Nothing

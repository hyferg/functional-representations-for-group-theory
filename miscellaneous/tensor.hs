idxSum a b = zipWith (+) a b

class Node a where
  contract :: a -> a -> a

data Casimir = CasimirList [Int]

instance Node Casimir where
  contract (CasimirList a) (CasimirList b) = CasimirList $ idxSum a b

genCas :: [Int] -> Casimir
genCas a = CasimirList $ a ++ (repeat 0)

instance Semigroup Casimir where
  (<>) = contract

instance Monoid Casimir where
  mempty = CasimirList (repeat 0)

instance Show Casimir where
  show (CasimirList a) =
    let idxs = [snd pairs | pairs <- zip a ['i'..'z'], fst pairs == 1]
    in "C_{" ++ idxs ++ "}"


x = genCas [1,1,1]
y = genCas [0,1,1,1]
z = genCas [0,0,0,1,1,1]

define = [x,y,z]
reduce = mconcat define

class Vector a where
  vAdd :: a -> a -> a
  vDot :: a -> a -> a

selfAddOnce :: (Vector v) => v -> v
selfAddOnce v = vAdd v v
selfAddN n v = (iterate selfAddOnce v) !! n

data VectorN = VectorN [Int] deriving Show
instance Vector VectorN where
  vAdd (VectorN a) (VectorN b) = VectorN $ zipWith (+) a b
  vDot (VectorN a) (VectorN b) = VectorN $ zipWith (*) a b

vCreate a = VectorN [a,a,a]

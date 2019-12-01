{-#LANGUAGE TemplateHaskell#-}

--import Control.Lens.Tutorial
import Control.Lens hiding (element)

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makeLenses ''Atom
makeLenses ''Point

--point :: Lens' Atom Point
-- point = lens _point (\atom newPoint -> atom { _point = newPoint })

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)


atom = Atom "A" (Point 0 0 )

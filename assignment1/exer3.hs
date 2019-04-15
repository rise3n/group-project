module Shape where

type Number = Integer
type Point = (Number,Number)
type Length = Number

data Shape = Pt Point 
	   | Circle Point Length 
	   | Rect Point Length Length 
	   deriving Show

type Figure = [Shape]

type BBox = (Point,Point)

----------c---------------
minX :: Shape -> Number
minX (Pt (x,y)) = x 
minX (Circle (x,y) l) = x-l
minX (Rect (x,y) l w) = x


---------d-----------------

move :: Shape -> Point -> Shape
move (Pt (x,y)) (vecx,vecy) = Pt (addPt (x,y) (vecx,vecy))
move (Circle (x,y) l) (vecx,vecy) = Circle (addPt (x,y) (vecx,vecy)) l
move (Rect (x,y) l w) (vecx,vecy) = Rect (addPt (x,y) (vecx,vecy)) l w

addPt :: Point -> Point -> Point
addPt (x1,y1) (x2,y2) = (x1+x2,y1+y2)


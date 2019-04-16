--Hongyuan Zhu
--Miao Zhou
--Haofeng Tian
--zunyue Qiu
--Yingfeng Fang

import HW1types

---------exercise1----------------------
type Bag a = [(a,Integer)]

ins :: Eq a => a -> Bag a -> Bag a
ins m [] = []
ins m ((x,poss):xs) = if m == x then (x,succ poss):xs
               else (x,poss):ins m xs
----------------b----------

del :: Eq a => a -> Bag a -> Bag a
del m [] = []
del m ((x,poss):xs) = if (m==x)&& (poss == 1) then xs
                      else if m==x then (x,poss-1):xs
		      else (x,poss):(del m xs)
---------------c-------------

bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = (x,count x (x:xs)): bag (remov x xs) 

count :: Eq a => a -> [a] -> Integer
count m [] = 0
count m (x:xs) = if (m == x) then  (count m xs) + 1
                      else count m xs

remov :: Eq a => a -> [a] -> [a]
remov m [] = []
remov m (x:xs) = if (m==x) then remov m xs  
                 else x:(remov m xs)
                      
------------d----------------

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag _ [] = False
subbag (x:xs) (y:ys) = if (equl x (y:ys)) then subbag xs (y:ys)
                       else False

equl :: Eq a => (a, Integer) -> Bag a -> Bool
equl _ [] = False
equl (x,px) ((y,py):ys) = if (x==y && px<=py) then True
			  else if(x==y && px>py) then False
			  else equl (x,px) ys

---------------e---------------

isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag [] _ = []
isbag _ [] = []
isbag (x:xs) (y:ys) = if (equl2 x (y:ys)) then isbag xs (y:ys) ++ [intersect x (y:ys)] 
		      else isbag xs (y:ys)

equl2 :: Eq a => (a, Integer) -> Bag a -> Bool
equl2 _ [] = False
equl2 (x,px) ((y,py):ys) = if (x==y) then True                           
                           else equl2 (x,px) ys

intersect :: Eq a => (a, Integer) -> Bag a -> (a, Integer)
intersect (x,px) [] = (x,px)
intersect (x,px) ((y,py):ys) = if (x==y && px<= py) then (x,px)
                               else if(x==y) then (x,py)
                               else intersect (x,px) ys


------------------------------exercise2-------------------------


g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

----------------a----------------

nodes :: Graph -> [Node]
nodes []=[]
nodes (x:xs)= norm(nodes xs ++ [fst x] ++ [snd x])
-----------------b--------------

suc :: Node -> Graph -> [Node]
suc n [] = []
suc n (x:xs) = if (fst x) == n then [snd x] ++ suc n xs
               else suc n xs

gsuccessor = suc 2 g
hsuccessor = suc 4 h
-----------------c--------------

detach :: Node -> Graph -> Graph
detach n [] = []
detach n (x:xs) = if (fst x) == n  || (snd x) == n then detach n xs
                  else [x] ++ detach n xs

gdetachedgraph = detach 3 g
hdetachedgraph = detach 2 h
----------------d---------------

cyc :: Int -> Graph
cyc n | n == 0 = []
      | n == 1 = [(1,1)]
      | n > 1 = zip [1..n] ([2..n]++[1])

---------------------------exercise3-------------------

type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point
            | Circle Point Length
	    | Rect Point Length Length
	    deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

-------a-----------
width :: Shape -> Length
width (Pt (x,y)) = 0
width (Circle (x,y) r) = 2*r
width (Rect (x,y) l w) = l

-----------b------------
bbox :: Shape -> BBox
bbox (Pt (x,y)) = ((x,y),(x,y))
bbox (Circle (x,y) r) = ((x-r,y-r),(x+r, y+r))
bbox (Rect (x,y) l w) = ((x,y),(x+l, y+w))

------------c---------------
minX :: Shape -> Number
minX (Pt (x,y)) = x
minX (Circle (x,y) l) = x-l
minX (Rect (x,y) l w) = x

-----------d-----------------
move :: Shape -> Point -> Shape
move (Pt (x,y)) (vecx,vecy) = Pt (addPt (x,y) (vecx,vecy))
move (Circle (x,y) l) (vecx,vecy) = Circle (addPt (x,y) (vecx,vecy)) l
move (Rect (x,y) l w) (vecx,vecy) = Rect (addPt (x,y) (vecx,vecy)) l w

addPt :: Point -> Point -> Point
addPt (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-----------e----------------

alignLeft :: Figure -> Figure
alignLeft [] = []
alignLeft ((Pt (x,y)):xs) = [moveToX 0 (Pt (x,y))] ++ alignLeft xs
-- move all shape to y axis
alignLeft ((Circle (x,y) r):xs) = [moveToX 0 (Circle (x,y) r)] ++ alignLeft xs
alignLeft ((Rect (x,y) l w):xs) = [moveToX 0 (Rect (x,y) l w)] ++ alignLeft xs

moveToX :: Number -> Shape -> Shape
moveToX a (Pt (x,y)) = move (Pt (x,y)) (a-x,0)
moveToX a (Circle (x,y) r) = move (Circle (x,y) r) (a-x+r,0)
moveToX a (Rect (x,y) l w) = move (Rect (x,y) l w) (a-x,0)

-----------------f------------
inside :: Shape -> Shape -> Bool
inside (Pt (x,y)) (Pt (m,n)) = if (x == m && y == n) then True
                               else False
inside (Pt (x,y)) (Circle (m,n) r) = if (((x-m)*(x-m)+(y-n)*(y-n)) <= r*r) then True
				     else False

inside (Circle (m,n) r) (Pt (x,y))= if (((x-m)*(x-m)+(y-n)*(y-n)) <= r*r) then True
				    else False
					         
inside (Pt (x,y)) (Rect (m,n) l w) = if (x <= m+w && y <= n+l) then True
				     else False

inside (Rect (m,n) l w) (Pt (x,y)) = if (x <= m+w && y <= n+l) then True
				     else False 
							   
inside (Circle (x,y) r) (Circle (m,n) r2) = if (r<= r2) && (m-r2 <= x-r) && (n-r2<=y-r) && (m+r2>=x+r) && (n+r2>=y+r) then True
					    else if (r>= r2) && (m-r2 >= x-r) && (n-r2>=y-r) && (m+r2<=x+r) && (n+r2<=y+r) then True
																		else False

inside (Rect (x,y) l w) (Rect (m,n) l1 w1) = if (x >= m && y >= n && x+w<=m+w1 && y+l<=n+l1) then True
				             else if (x <= m && y <= n && x+w>=m+w1 && y+l>=n+l1) then True
					     else False

inside (Rect (x,y) l w) (Circle (m,n) r) = if (m-r>=x && n-r>=y && m+r<=x+w && n+r<=y+l)
then True
else False    

inside (Circle (m,n) r) (Rect (x,y) l w) = if (m-r>=x && n-r>=y && m+r<=x+w && n+r<=y+l)
then True
else False 



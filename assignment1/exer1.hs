import Debug.Trace

---------exercise1-----------
type Bag a = [(a,Integer)]

ins :: Eq a => a -> Bag a -> Bag a
ins m [] = []
ins m ((x,poss):xs) = if m == x then (x,succ poss):xs
               else (x,poss):ins m xs
----------------b----------

del :: Eq a => a -> Bag a -> Bag a
del m [] = []
del m ((x,poss):xs) = if (m==x)&& (poss == 1) then xs
                      else if m==x then (x,poss-1):xs                                                                          else del m xs
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
remov m (x:xs) = if (count m (x:xs) /= 0) then remov m xs  
                          else x:xs
                      
------------d----------------

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
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




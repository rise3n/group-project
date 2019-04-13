-- this is exercise two
import HW1types 

g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

firstnode :: Edge -> Node
firstnode (x,_) = x

secondnode :: Edge -> Node
secondnode (_,x) = x

----------------a----------------

nodes :: Graph -> [Node] 
nodes []=[]
nodes (x:xs)= nodes xs ++ [firstnode x] ++ [secondnode x]

gnetlist = norm (nodes g)
hnetlist = norm (nodes h)
-----------------b--------------

suc :: Node -> Graph -> [Node]
suc n [] = []
suc n (x:xs) = if (firstnode x) == n then [secondnode x] ++ suc n xs
               else suc n xs

gsuccessor = suc 2 g               
hsuccessor = suc 4 h
-----------------c--------------

detach :: Node -> Graph -> Graph
detach n [] = []
detach n (x:xs) = if (firstnode x) == n  || (secondnode x) == n then detach n xs
                  else [x] ++ detach n xs

gdetachedgraph = detach 3 g
hdetachedgraph = detach 2 h
----------------d---------------

cyc :: Int -> Graph
cyc 1 = []
cyc n = cyc(n-1) ++ [((n-1),n)] 

cycgraph = cyc 4 ++ [(4,1)] 
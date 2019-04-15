-- this is exercise two
import HW1types

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

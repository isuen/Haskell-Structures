module Prim where


import System.IO
import System.Environment

----- DATA DEFINITION: This implementation of graph uses adjacency list ----- 
data Vertex = Vertex String [String] Int String deriving (Show)

-- each vertex has a name (label)
label :: Vertex -> String
label (Vertex label _ _ _) = label
-- a list of vertices adjacent to current vertex
adjacency :: Vertex -> [String]
adjacency (Vertex _ adjacency _ _) = adjacency
-- a distance from current vertex to root
distance :: Vertex -> Int
distance (Vertex _ _ distance _) = distance
-- predecessor of current vertex
predecessor :: Vertex -> String
predecessor (Vertex _ _ _ predecessor) = predecessor

-- an edge connects two vertices 
data Edge = Edge (Vertex,Vertex) Int deriving (Show)

weight :: Edge -> Int
weight (Edge _ weight) = weight

v1 :: Edge -> Vertex
v1 (Edge (v1,_) _) = v1

v2 :: Edge -> Vertex
v2 (Edge (_,v2) _) = v2

-- graph contains a list of vertex and a list of edges--
data Graph = Graph [Vertex] [Edge] deriving (Show)


----- prim's algorithm minimum spanning tree-----

----accessors----

-- determine if the a given vertex is a member of the list of vertices
hasVertex _ []  = False
hasVertex v (x:y) 
     | label v == x = True
     | otherwise = hasVertex v y

-- get a list of all vertices from a graph according to labels
labelsToVertices (Graph [] _) _ = []
labelsToVertices (Graph (x:y) _) [] = x:y
labelsToVertices (Graph (x:y) _) labels = filter (\ a -> label a `elem` labels) (x:y)
-- If either `s` (seen) or `vn` (vertex neighbors) are empty, return an empty list.
removeLabeled _ [] = []
removeLabeled [] _ = []
-- Filter out all vertexes in `vn` that are also in `s`.
removeLabeled q l = filter (\ x -> not $ hasVertex x l) q

-- Go though each vertex and swap the current distance and predecessor labels with the new parameters.
updatePred (Vertex l a d p) vLabel = (Vertex l a d vLabel)

-- initialize graph to label evey vertex distance zero
setDist lst = [ (Vertex s lst  0 s2) | (Vertex s lst int s2)<- lst]

merge vLabel egIn = [(Edge(a,b) i) | (Edge (a,b) i) <- egIn, 
          (label a == vLabel || label b == vLabel)]

{-
(notInPairList (a,b) (getPairs seen)))]
-}
notInPairList (a,b) [] = True

notInPairList (a,b) ((x,y):rest) 
     | (label a == label x) && (label b == label y) = False
     | otherwise = notInPairList (a,b) rest

getPairs eList = [(x,y) | (Edge (x,y) i) <- eList]


findMin (Graph (x:y) (xe:ye)) labeled l = 
     edgeMin [(Edge (a,b) i) | (Edge (a,b) i) <- (xe:ye), 
               ((label a ==l && (label b `notElem` vertexToLabel labeled)) || (label b ==l && (label a `notElem` vertexToLabel labeled)))] 
               xe

vertexToLabel []= []
vertexToLabel lst = [label x | x <- lst]


getMinEdge [] _ = error "list cannot be empty"
getMinEdge (x:y) [] = edgeMin (x:y) x
getMinEdge le lv = edgeMin (removeMin le lv) (head (removeMin le lv))

removeMin le lv = [(Edge(a,b) i) | (Edge (a,b) i) <- le, 
          ((label a `notElem` lv) || (label b `notElem` lv))]

edgeMin [] min = min

edgeMin (a:b) min 
     | (weight a <= weight min) = edgeMin b a
     | otherwise = edgeMin b min

toNextVertex (Edge (a,b) _) lst
     | ((label a) `elem` lst) = b
     | otherwise = a

toPrevVertex (Edge (a,b) _) lst
     | ((label a) `elem` lst) = a
     | otherwise = b

--- Prim's algorithm ---
---prim :: Graph -> Graph -> [Vertex] -> [Vertex] -> Vertex -> Graph
--base case :empty graph

--prim (Graph [] lst) _ _ _ _ _ = Graph [] lst

--when reached empty queue
prim _ outGraph [a] vertex seenEdge seenVertex = outGraph

prim (Graph verIn egIn) (Graph ver eg) q vi se sv = prim inGraph outGraph queue v seenEdge seenVertex
    where inGraph = Graph verIn egIn
          -- Get the current vertex label.
          vLabel = label vi
          -- add seen vertex label into list of seenV
          seenV = vLabel:sv
          -- add seen edge into list of seenE which accumulated as we are traversing
          seenE = (merge vLabel egIn)++se
 
          -- find the minimun weighted edges from list seen so far
          minEdge = getMinEdge seenE seenV           
          -- minEdge = findMin inGraph labeled vLabel
          
          -- get the next vertex we haven't seen 
          nextVertex = toNextVertex minEdge seenV
          
          prevVertex = toPrevVertex minEdge seenV
          
          addToGraph = updatePred nextVertex (label prevVertex)

-- update accumulators for the next loop
          outGraph = Graph (addToGraph:ver) (minEdge:eg) 
          v = nextVertex
          queue = removeLabeled q seenV
          seenEdge = seenE
          seenVertex = seenV
          --queue = removeLabeled q labeled' 
          -- ++(nextVertex:[]))


bar = Vertex "Barcelona" ["London","Milan","Paris","Madrid"] 1000 ""
mil = Vertex "Milan" ["Prague","Berlin","Paris","Barcelona"] 1000 ""
mad = Vertex "Madrid" ["Barcelona","London"] 1000 ""
pra = Vertex "Prague" ["Milan","Berlin"] 1000 ""
par = Vertex "Paris" ["Milan","Barcelona","London","Berlin"] 1000 ""
lon = Vertex "London" ["Stockholm","Berlin","Paris","Barcelona","Madrid"] 1000 ""
ber = Vertex "Berlin" ["Prague","Milan","Paris","London","Warsaw","Helsinki","Stockholm"] 1000 ""
war = Vertex "Warsaw" ["Berlin","Helsinki"] 1000 ""
hel = Vertex "Helsinki" ["Warsaw","Berlin","Stockholm"] 1000 ""
sto = Vertex "Stockholm" ["Helsinki","Berlin","London"] 1000 ""

barmil = Edge (bar, mil) 451
barpar = Edge (bar, par) 515
barlon = Edge (bar, lon) 707
barmad = Edge (bar, mad) 314
milpar = Edge (mil, par) 398
parlon = Edge (par, lon) 213
madlon = Edge (mad, lon) 785
milpra = Edge (mil, pra) 398
milber = Edge (mil, ber) 524
parber = Edge (par, ber) 546
berlon = Edge (ber, lon) 580
praber = Edge (pra, ber) 174
stolon = Edge (sto, lon) 892
warber = Edge (war, ber) 312
helber = Edge (hel, ber) 687
stober = Edge (sto, ber) 504
warhel = Edge (war, hel) 570
helsto = Edge (hel, sto) 246

osa = Vertex "Osaka" ["Kyoto"] 1000 ""
kyo = Vertex "Kyoto" ["Osaka", "Nagoya", "Tokyo"] 1000 ""
nag = Vertex "Nagoya" ["Kyoto", "Tokyo"] 1000 ""
tok = Vertex "Tokyo" ["Kyoto", "Nagoya"] 1000 ""

osakyo = Edge (osa,kyo) 27
kyonag = Edge (kyo,nag) 65
kyotok = Edge (kyo,tok) 226
toknag = Edge (tok,nag) 160


europe_edge = Graph [bar,mil,mad,pra,par,lon,ber,war,hel,sto] [barmil,barpar,barlon,barmad,milpar,parlon,madlon,milpra,milber,parber,berlon,praber,stolon,warber,helber,stober,warhel,helsto] 



mst = 
	do
	    	
          let result = prim (Graph [ber,war,hel,sto] [warber,helber,stober,warhel,helsto]) (Graph [ber] []) [ber,war,hel,sto] ber [] [] 
		print result
		return()




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

data Graph = Graph [Vertex] [Edge] deriving (Show)


----- Prim's theorem minimum spanning tree-----

----accessors----

-- determine if the a given vertex is a member of the list of Labels of vertices
hasVertex :: Vertex -> [String] -> Bool
hasVertex _ []  = False
hasVertex v (x:y) 
     | label v == x = True
     | otherwise = hasVertex v y

-- remove vertices whose label is in list of label l from list of vertices q
removeLabeled :: [Vertex] -> [String] -> [Vertex]
removeLabeled _ [] = []
removeLabeled [] _ = []
removeLabeled q l = filter (\ x -> not $ hasVertex x l) q

-- update each labeled vertex by adding their predecessor, they are on either end of the minimum weighed edge
updatePred :: Vertex -> String -> Vertex
updatePred (Vertex l a d p) pred = (Vertex l a d pred)

-- get a list of edges from a given vertex -- 
getEdgeList :: String -> [Edge] -> [Edge]
getEdgeList vLabel egIn = [(Edge(a,b) i) | (Edge (a,b) i) <- egIn, 
          (label a == vLabel || label b == vLabel)]

-- can potentially be used for implementing a set to log all minimum edges--
-- a set is not implemented here --
notInPairList (a,b) [] = True

notInPairList (a,b) ((x,y):rest) 
     | (label a == label x) && (label b == label y) = False
     | otherwise = notInPairList (a,b) rest

getPairs eList = [(x,y) | (Edge (x,y) i) <- eList]

-- call to find the minimum weighed edge from a list of edges that has been seen but not used
getMinEdge :: [Edge] -> [String] -> Edge
getMinEdge [] _ = error "list cannot be empty"
getMinEdge (x:y) [] = edgeMin (x:y) x
getMinEdge le lv = edgeMin (removeMin le lv) (head (removeMin le lv))

-- filter out edges that connects two already labeled vertices-- 
removeMin :: [Edge] -> [String] -> [Edge]
removeMin le lv = [(Edge(a,b) i) | (Edge (a,b) i) <- le, 
          ((label a `notElem` lv) || (label b `notElem` lv))]

-- find the minimum weighted edge -- 
edgeMin :: [Edge] -> Edge -> Edge

edgeMin [] min = min
edgeMin (a:b) min 
     | (weight a <= weight min) = edgeMin b a
     | otherwise = edgeMin b min

-- access vertex from edge 
toNextVertex :: Edge -> [String] -> Vertex

toNextVertex (Edge (a,b) _) lst
     | ((label a) `elem` lst) = b
     | otherwise = a

toPrevVertex :: Edge -> [String] -> Vertex

toPrevVertex (Edge (a,b) _) lst
     | ((label a) `elem` lst) = a
     | otherwise = b

--- Prim's algorithm ---
-- argument: input graph, output graph accumulator, a queue initalized with all vertices in a graph,
-- a starting vertex, an edge list accumulator keeping the incident edges of every vertex seen so far
-- a list of vertices that has been reached by the minimum spannig tree
prim :: Graph -> Graph -> [Vertex] -> Vertex -> [Edge] -> [String] -> Graph
--base case :empty graph
prim (Graph [] lst) _ _ _ _ _ = Graph [] lst

--when reaching queue containing only one element, meaning all vertices has been travered, return outGraph accumulator
prim _ outGraph [a] vertex seenEdge seenVertex = outGraph

prim (Graph verIn egIn) (Graph ver eg) q vi se sv = prim inGraph outGraph queue v seenEdge seenVertex
    where inGraph = Graph verIn egIn
          -- Get the current vertex label.
          vLabel = label vi
          -- add this vertex label into list of seenV
          seenV = vLabel:sv
          -- add all incident edges of current vertex into list of seenE
          seenE = (getEdgeList vLabel egIn)++se 
          -- find the minimun weighted edges from list of unused edges seen so far
          minEdge = getMinEdge seenE seenV                     
          -- get the next vertex we haven't seen 
          nextVertex = toNextVertex minEdge seenV
          -- get its predecessor
          prevVertex = toPrevVertex minEdge seenV
          -- update this vertex by addeing its predecessor
          addToGraph = updatePred nextVertex (label prevVertex)

          -- update accumulators for the next loop
          outGraph = Graph (addToGraph:ver) (minEdge:eg) 
          v = nextVertex
          --remove vertices that has been labeled
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

europe_edge = Graph [bar,mil,mad,pra,par,lon,ber,war,hel,sto] [barmil,barpar,barlon,barmad,milpar,parlon,madlon,milpra,milber,parber,berlon,praber,stolon,warber,helber,stober,warhel,helsto] 

osa = Vertex "Osaka" ["Kyoto"] 1000 ""
kyo = Vertex "Kyoto" ["Osaka", "Nagoya", "Tokyo"] 1000 ""
nag = Vertex "Nagoya" ["Kyoto", "Tokyo"] 1000 ""
tok = Vertex "Tokyo" ["Kyoto", "Nagoya"] 1000 ""

osakyo = Edge (osa,kyo) 27
kyonag = Edge (kyo,nag) 65
kyotok = Edge (kyo,tok) 226
toknag = Edge (tok,nag) 160

getEV (Graph _ le) = [[(label a), (label b), show(i)] | (Edge (a,b) i) <- le] 

mst = 

    do
         putStrLn "Please enter the map to load:"
         putStrLn "(1.Europe 2.Japan)"
         country <- getLine	    
         let result = (choose country)
         let edges = getEV result
         print edges
         return()


choose a 
     | a == "1" = prim (Graph [ber,war,hel,sto] [warber,helber,stober,warhel,helsto]) (Graph [ber] []) [ber,war,hel,sto] ber [] []
     | otherwise = prim (Graph [osa,kyo,nag,tok] [osakyo,kyonag,kyotok,toknag]) (Graph [osa] []) [osa,kyo,nag,tok] osa [] []

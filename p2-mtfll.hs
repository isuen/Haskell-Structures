--CPSC 312 Project 2
-- Imelda Suen, Racheal Zhao, Joshua Zhou
module Mtfll where

import System.Random 
import System.IO.Unsafe
--useful for testing: create a list of random numbers of length new

randomList n = [unsafePerformIO (getStdRandom (randomR (1,100))) | x <- [1..n]]

-- MTF linked list; linked list the moves the found element to the front on find.
--Data type definition
data Mtfll t =
	Empty |
	Node t (Mtfll t)

--Set up display for Mtfll.
instance (Show t) => Show (Mtfll t) where
 show l = show $ mtfll_toList l

--Easy conversion from a list of tuples.
mtfll_fromList :: [t] -> Mtfll t 
mtfll_fromList (h:t) = Node h (mtfll_fromList t)
mtfll_fromList [] = Empty

--converts mtfll to a list
mtfll_toList :: Mtfll t -> [t]
mtfll_toList Empty = []
mtfll_toList (Node v r) = v:(mtfll_toList r)

--Insert a value. Simply appended to the front.
mtfll_insert :: Mtfll t -> t -> Mtfll t
mtfll_insert l val = (Node val l)



-- llappend: appends two linked lists together, so the 'empty' of l1 points to l2.
llappend :: Mtfll t -> Mtfll t -> Mtfll t
llappend Empty l2 = l2
llappend (Node h r) l2 = Node h $ llappend r l2

--Finds a value in the list, if it exists.
--Rearranges to the front.
--Note that in a move-to-front linked list, the rearrangement is considered a 'side effect'!
--Need to simulate state by returning two values; the found value, and the new linked list.
mtfll_find :: Eq t => Mtfll t -> t -> (Maybe t, Mtfll t)
mtfll_find l v = 
	mtfll_findhelper l v Empty

--findhelper works as a find but keeps track of the nodes seen so far 
--so it can reconstruct the rearranged l
mtfll_findhelper :: Eq t => Mtfll t -> t -> Mtfll t -> (Maybe t, Mtfll t)
mtfll_findhelper Empty t acc = (Nothing, acc)
mtfll_findhelper (Node v r) t acc 
	| v==t = (Just v, (Node v (llappend acc r)))
	| otherwise = mtfll_findhelper r t (llappend acc (Node v Empty))


--sorts a mtf-linked list.
--Use a mergesort strategy.
mtfll_sort :: Ord t => Mtfll t -> Mtfll t
mtfll_sort Empty = Empty
mtfll_sort (Node v Empty) = (Node v Empty)
mtfll_sort (Node v1 (Node v2 Empty)) = 
	if (v1 < v2) then (Node v1 (Node v2 Empty)) else (Node v2 (Node v1 Empty))
--if more than 2 nodes!
mtfll_sort (Node v1 (Node v2 (Node v3 r))) = mtfll_merge  (mtfll_sort  (fst split)) (mtfll_sort (snd split))
	where 
	split = frontBackSplit (Node v1 (Node v2 (Node v3 r)))
	


--frontBackSplit splits a linked list into two different linked lists.
frontBackSplit :: Mtfll t -> (Mtfll t, Mtfll t)
frontBackSplit l = frontBackSplitHelper l l Empty

--frontBackSplitHelper:
--use fast-slow lane strategy
--one is 'fast', the other is 'slow'
--acc keeps track of the nodes seen so far
--case 1: at least 2 nodes.
frontBackSplitHelper :: Mtfll t -> Mtfll t -> Mtfll t -> (Mtfll t, Mtfll t)
frontBackSplitHelper Empty l2 acc = (acc,l2)
frontBackSplitHelper (Node v1 (Node v2 r1)) (Node v3 r2) acc = frontBackSplitHelper r1 r2 $ llappend acc $ Node v3 Empty
frontBackSplitHelper (Node v1 Empty) l2 acc = (acc, l2) --split so extra element is dumped in 2nd list

--mtfll_merge: merges two in-order linked lists.
mtfll_merge :: Ord t => Mtfll t -> Mtfll t -> Mtfll t
mtfll_merge Empty Empty = Empty
mtfll_merge (Node v1 r1) Empty = (Node v1 r1)
mtfll_merge Empty (Node v2 r2) = (Node v2 r2)
mtfll_merge (Node v1 r1) (Node v2 r2) = 
	if (v1 < v2) then (Node v1 $ mtfll_merge r1 $ Node v2 r2)
	else Node v2 $ mtfll_merge (Node v1 r1) r2

--update: updates an element at position x.
--if position > length, then ignore.
mtfll_update :: Mtfll t -> Int -> t -> Mtfll t
mtfll_update Empty _ _  = Empty
mtfll_update (Node val r) pos newval
	| pos == 0 = (Node newval r)
	| otherwise = (Node val (mtfll_update r (pos-1) newval))

--delete: deletes an element of position x.
mtfll_delAt :: Mtfll t -> Int -> Mtfll t
mtfll_delAt Empty _ = Empty
mtfll_delAt (Node val r) pos
	| pos==0 = r
	| otherwise = (Node val (mtfll_delAt r (pos-1)))
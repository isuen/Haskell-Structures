-- CPSC 312 Project 2
-- Imelda Suen, Racheal Zhao, Joshua Zhou

module RAList where

--data definition; random-access list
--list of binary trees, each of increasing length
--N elements has log(n) trees; each tree is at most log(n) depth
--Each root node has a value determining the size of the tree
--ALL BINARY TREES MUST BE COMPLETE!
data RABTree a = 
	RALeaf a |
	RANode a (RABTree a) (RABTree a) deriving (Show, Eq)

type RAList a = [(Int, RABTree a)]

--note: empty is still []

--join two equally-sized complete surface level-trees (with their sizes) into a new tree.
--with the node value specified
--i.e. only called on entire elements of the RAList array
rabtree_join :: (Int, RABTree a) -> (Int, RABTree a) -> a -> (Int, RABTree a)
rabtree_join (v1, t1) (v2, t2) v
	| v1 == v2 = ((v1+v2),(RANode v t1 t2))
	| otherwise = error "can't join two trees of different sizes"

--inserting a value
--note: stored in reverse so insertion is O(1)
ralist_insert :: RAList a -> a -> RAList a
--case 1: empty list
ralist_insert [] v = [(1, RALeaf v)]
--case 2: one element
--since the other tree is complete, this means that this has to be a separate element
ralist_insert [(n,t)] v  = [(1, RALeaf v),(n,t)]
--case 3: two or more trees. then we may have to worry about appending.
ralist_insert l@(bt1@(n1,t1):bt2@(n2,t2):r) v =
	if n1==n2
	--trees same size: join front two, prepend to list.
	then (n1+n2+1,(snd (rabtree_join bt1 bt2 v))) : r
	else (1, RALeaf v):l

--can finally make fromList!
ralist_fromList :: [a] -> RAList a
ralist_fromList l = foldr (\elem ral -> (ralist_insert ral elem)) [] (reverse l)

--delLast
--delete the last element
ralist_delLast :: RAList a -> RAList a
ralist_delLast ((_, RALeaf _):rest) = rest
ralist_delLast ((n, RANode _ l r):rest) = (n2,l):(n2,r):rest
	where n2 = (n-1) `div` 2
--length: get the length of a random access list.
ralist_length :: RAList a -> Int
ralist_length = foldr (\(size, tree) totalsize -> size+totalsize) 0

--get :: gets the ith element of the list. may throw error.
ralist_get :: RAList a -> Int -> a
ralist_get rl n = ralist_getfromback rl ((ralist_length rl)-n)  

--getfromback: helper. gets from back.
ralist_getfromback :: RAList a -> Int -> a
ralist_getfromback [] _ = error "subscript out of bounds"
ralist_getfromback ((size, bt):r) n
	| n < size = rabtree_findintree bt size n
	| n >= size = ralist_getfromback r (n-size)

--traverses the tree and gets the nth element of the tree
--size of node is also passed in; inferred from the RAList that holds this tree
--depth-first, preorder traversal
rabtree_findintree :: RABTree a -> Int -> Int -> a
rabtree_findintree (RANode v lt rt) size n 
	| n == 0 = v
	| otherwise = 
		-- split into two; calculate sizes, and make decision on which subtree to each
		if (size <= n) 
		then rabtree_findintree lt subsize (n-1)
		else rabtree_findintree rt subsize (n-subsize-1)
		where subsize = (size-1) `div` 2
rabtree_findintree (RALeaf v) _  n 
	| n == 0 = v
	| otherwise = error "error in pre-order traversal - wrong size computed"

-- update :: updates the nth element of a RAList.
-- note: stored in reverse, so 'front' of the array is actually the last element!
ralist_update :: RAList a -> Int -> a -> RAList a
ralist_update rl n newval= ralist_updatefromback rl ((ralist_length rl)-n) newval

--getfromback: helper. gets from back.
ralist_updatefromback :: RAList a -> Int -> a -> RAList a
ralist_updatefromback [] _  _ = error "subscript out of bounds"
ralist_updatefromback ((size, bt):r) n newval
	| n < size = (size,(rabtree_updateintree bt size n newval)):r
	| n >= size = (size,bt):(ralist_updatefromback r (n-size) newval)

--traverses the tree and gets the nth element of the tree
--size of node is also passed in; inferred from the RAList that holds this tree
--depth-first, preorder traversal
rabtree_updateintree :: RABTree a -> Int -> Int -> a -> RABTree a
rabtree_updateintree (RANode v lt rt) size n newval
	| n == 0 = (RANode newval lt rt)
	| otherwise = 
		-- split into two; calculate sizes, and make decision on which subtree to each
		if (size <= n) 
		then (RANode v (rabtree_updateintree lt subsize (n-1) newval) rt)
		else (RANode v lt (rabtree_updateintree rt subsize (n-subsize-1) newval))
		where subsize = (size-1) `div` 2
rabtree_updateintree (RALeaf v) _  n newval
	| n == 0 = (RALeaf newval)
	| otherwise = error "error in pre-order traversal - wrong size computed"
--CPSC 312 Project 2
-- Imelda Suen, Rachel Zhao, Joshua Zhou

-- AVL Tree
-- height-balanced trees

-- interp. a AVLTree with key, value, height, left tree and right tree
-- height of an empty tree is -1
data AVLTree k v = 
    Empty | Node k v Int (AVLTree k v) (AVLTree k v)
    deriving (Show)
  

-- tolist lst returns list giving inorder traversal using accumulators
tolist :: AVLTree k v -> [(k,v,Int)]

tolist lst = tolisth lst []
    where
        tolisth Empty acc = acc
        tolisth (Node key val h lt rt) acc = 
            tolisth lt ((key,val,h) : tolisth rt acc)

-- inserts key val into AVLTree
insert :: (Ord k) => k -> v -> AVLTree k v -> AVLTree k v         
insert key val Empty = Node key val 0 Empty Empty
insert key val (Node k1 v1 h lt rt)
    | key == k1 = Node key val h lt rt
    | key < k1 = balance(Node k1 v1 h (insert key val lt) rt)
    | key > k1 = balance(Node k1 v1 h lt (insert key val rt))


balance Empty = Empty
balance (Node k v h lt rt)
    | (bf >= -1) && (bf <= 1) = Node k v ((max (height lt) (height rt))+1) lt rt
    | bf <= -2 = lbalance (Node k v h lt rt) lb
    | bf >= 2 = rbalance (Node k v h lt rt) rb
    where 
        bf = bfactor (Node k v h lt rt)
        lb = bfactor lt
        rb = bfactor rt

lbalance avltree lb
    | lb == -1 = rotateR avltree
    | otherwise = rotateLR avltree
    where
        rotateR (Node k v h (Node kl vl hl ltl rtl) rt) =
            (Node kl vl hl ltl (Node k v ((max (height rtl) (height rt))+1) rtl rt))
        rotateL (Node k v h lt (Node kr vr hr ltr rtr)) =
            (Node kr vr (hr+1) (Node k v ((max (height lt) (height ltr))+1) lt ltr) rtr)
        rotateLR (Node k v h lt rt) = 
            rotateR(Node k v h (rotateL lt) rt)

rbalance avltree rb
    | rb == 1 = rotateL avltree
    | otherwise = rotateRL avltree
    where
        rotateR (Node k v h (Node kl vl hl ltl rtl) rt) =
            (Node kl vl (hl+1) ltl (Node k v ((max (height rtl) (height rt))+1) rtl rt))
        rotateL (Node k v h lt (Node kr vr hr ltr rtr)) =
            (Node kr vr hr (Node k v ((max (height lt) (height ltr))+1) lt ltr) rtr)
        rotateRL (Node k v h lt rt) =
            rotateL(Node k v h lt (rotateR rt))

height Empty = -1
height (Node _ _ h _ _) = h

bfactor Empty = 0
bfactor (Node k v h lt rt) = (height rt) - (height lt)
        
-- Example trees
avl1 = (Node 7 "7" 2 (Node 2 "2" 1 (Node 1 "1" 0 Empty Empty) Empty) (Node 15 "15" 0 Empty  Empty))
avl2 = insert 18 "18" avl1
-- rotateL called
avlL1 = insert 19 "19" avl2
-- rotateL bigger example
avlL2 = insert 6 "6" (insert 5 "5" (insert 3 "3" (insert 4 "4" avlL1)))
-- rotateR 
avlR1 = insert 0 "0" avl1
-- rotateR bigger example
avlR2 = insert 11 "11" (insert 12 "12" (insert 14 "14" (insert 13 "13" avl2)))

-- rotateLR 
avlLR = insert 3 "3" (insert 4 "4" avl1)
-- rotateRL
avlRL = insert 13 "13" (insert 12 "12" (insert 10 "10" (insert 11 "11" avl2)))


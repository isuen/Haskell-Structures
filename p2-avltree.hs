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

-- deletes node with given key from AVLTree    
delete :: (Ord k) => k -> AVLTree k v -> AVLTree k v
delete _ Empty = Empty
delete key (Node k v h lt rt)
    | key > k = balance(Node k v h lt (delete key rt))
    | key < k = balance(Node k v h (delete key lt) rt)
    | (key == k) = removeEqKey key (Node k v h lt rt)
    
-- removes key when key is equal to the key of the given node
-- if no children: delete node to give Empty
-- if one child: give the child
-- if two children: finds min value in rt (keep going left until Empty),
--                  creates tree with min key,value in root,
--                  and removes min key from rt 
removeEqKey _ (Node _ _ _ Empty Empty) = Empty
removeEqKey _ (Node _ _ _ lt Empty) = lt
removeEqKey _ (Node _ _ _ Empty rt) = rt
removeEqKey key (Node k v h lt rt) = 
    balance (Node kmin vmin h lt (delete kmin rt))
    where
        (kmin, vmin) = findMin key rt
        findMin key (Node k v h Empty rt) = (k,v)
        findMin key (Node k v h lt rt) = findMin key lt

-- rebalances tree as necessary
-- rebalancing is necessary if difference between heights of left/right trees is 2
balance Empty = Empty
balance (Node k v h lt rt)
    | (bf >= -1) && (bf <= 1) = Node k v ((max (height lt) (height rt))+1) lt rt
    | bf <= -2 = lbalance (Node k v h lt rt) lb
    | bf >= 2 = rbalance (Node k v h lt rt) rb
    where 
        bf = bfactor (Node k v h lt rt)
        lb = bfactor lt
        rb = bfactor rt

-- for balancing if left side is causing imbalance
-- if left child's balance is -1 (its left child taller than right child), rotate right
-- if left child's balance is 1 (it's right child taller than left child), rotate left child left, then root right
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

-- for balancing if right side is causing imbalance
-- if right child's balance is -1 (its left child taller than right child), rotate left
-- if right child's balance is 1 (it's right child taller than left child), rotate right child right, then root left
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

-- produces height of given tree
height Empty = -1
height (Node _ _ h _ _) = h

-- produces difference between height of right child - left child
bfactor Empty = 0
bfactor (Node k v h lt rt) = (height rt) - (height lt)
        
-- Example trees
-- ===insertion===
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

-- ===deletion===
-- no child
avlNC = delete 10 avlRL
-- one child - left
avlLC = delete 2 avlRL
-- one child - right, B: with rebalance
avlRC = delete 15 avl2
avlRCB = delete 7 avl1
-- two children
avlTC = delete 7 avlRL
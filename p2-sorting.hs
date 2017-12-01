--CPSC 312 Project 2
-- Imelda Suen, Rachel Zhao, Joshua Zhou

-- some sorting algorithms implemented in Haskell

-- =============== INSERTION SORT ===============
-- insertion sort
-- inserts element at front of unsorted part of list at the correct sorted position
isort :: (Ord a) => [a] -> [a]

isort [] = []
isort (h:t) = insert h (isort t)
    where 
        insert :: (Ord a) => a -> [a] -> [a]
        insert x [] = [x]
        insert x (h:t)
            | x < h = x:(h:t)
            | otherwise = h : insert x t



-- =============== SELECTION SORT ===============
-- selection sort

ssort :: (Ord a) => [a] -> [a]

ssort [] = []
ssort lst = my_min lst : ssort (remove (my_min lst) lst)
    where 
        remove :: (Ord a) => a -> [a] -> [a] 
        remove x (h:t)
            | x == h = t
            | otherwise = h : remove x t

my_min :: (Ord a) => [a] -> a
my_min (h:t) = foldr (\ x y -> if x < y then x else y) h t

-- =============== MERGE SORT ===============
-- merge sort
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [a] = [a]
msort lst =  merge (msort (fst(split lst))) (msort (snd(split lst)))

-- split will split lst in half
-- if length of list is odd, second half will 1 more element
split :: [a] -> ([a],[a])
split lst = splith ([],lst) (div (length lst) 2)
    where 
        splith :: ([a],[a]) -> Int -> ([a],[a])
        splith r 0 = r
        splith (f,(h:t)) n = splith ((f++[h]),t) (n-1)

-- merge combines two sorted lists lst1 lst2 into one sorted list
merge :: (Ord a) => [a] -> [a] -> [a]       
merge lst1 [] = lst1
merge [] lst2 = lst2
merge (h1:t1) (h2:t2)
    | h1 <= h2 = h1 : merge t1 (h2:t2)
    | otherwise = h2 : merge (h1:t1) t2
        

-- =============== QUICK SORT ===============
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (h:t) = (qsort [x | x <- t, x <= h]) ++ [h] ++ qsort([x | x <- t, x > h])




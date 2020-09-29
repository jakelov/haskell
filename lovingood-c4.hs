-- Jake Lovingood
-- Homework 4:: Recursion over Trees
-- 9/15/2020
-- CS 3490


data LTree a = LLeaf a | LNode (LTree a) a (LTree a) deriving Show

mytree :: LTree Integer
mytree = LNode (LNode (LLeaf (-10)) (-6) (LLeaf 2)) 4 (LNode (LLeaf 7) 9 (LLeaf 10))

-- Problem Set 1
-- Implement each of the following functions, using recursion and patten-matching.

-- Compute the smallest integer occuring in the tree of integers.
minTree :: LTree Integer -> Integer
minTree (LLeaf x) = x
minTree (LNode lt x rt) = min (minTree lt) (min x (minTree rt))

-- Return the count of number of leaves in a tree.
countLeaves :: LTree a -> Integer
countLeaves (LLeaf x) = 1
countLeaves (LNode lt x rt) = (countLeaves lt) + (countLeaves rt)

-- Count Even Nodes: Count the number of Nodes that are not leaves and are even integers.
countEvenNodes :: LTree Integer -> Integer
countEvenNodes (LLeaf x) = 0
countEvenNodes (LNode lt x rt)
               | even x = 1 + countEvenNodes lt + countEvenNodes rt
               | otherwise = countEvenNodes lt + countEvenNodes rt

-- Returns True if first arg occurs among the leaves of a tree given in 2nd arg, and false otherwise.
occursInLeaves :: (Eq a) => a -> LTree a -> Bool
occursInLeaves x (LLeaf y) = (x == y)
occursInLeaves x (LNode lt y rt) = (occursInLeaves x lt) || (occursInLeaves x rt)

-- checkNoCover 
-- which returns True if the first argument occurs among the leaves of the tree given
-- in the second argument, and does not occur on any node above that leaf.
-- It returns False if every occurrence in the leaves is ”covered” by another occurrence higher in the tree.
checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover x (LLeaf y) = (x == y)
checkNoCover x (LNode lt y rt)
               | (x == y) = False
               | otherwise = (checkNoCover x lt) || (checkNoCover x rt)


-- Part Two: Non-Recursion/Folding Solutions
-- All functions from above but using treefolds to simulate the same solution.
-- FOLD TREE FORMAT FOR FOLDING.
foldTree :: (b -> a -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode t1 y t2) = comb (foldTree comb base t1) y (foldTree comb base t2)

-- MINTREE USING FOLDS
minTree' :: LTree Integer -> Integer
minTree' = foldTree (\l a r -> min(a) (min l r)) (\x -> x)

-- CountLeaves USING FOLDS
countLeaves' :: LTree a -> Integer
countLeaves' = foldTree (\l x r -> l + r) (\x -> 1)

-- CountEvenNodes USING FOLDS
countEvenNodes' :: LTree Integer -> Integer
countEvenNodes' = foldTree (\l x r -> l + (if even x then 1 else 0) + r) (\x -> 0)

-- OccursInLeaves USING FOLDS
occursInLeaves' :: (Eq a) => a -> LTree a -> Bool
occursInLeaves' n = foldTree (\l x r -> l || r) (\x -> n == x)

-- CheckNoCover USING FOLDS
checkNoCover' :: (Eq a) => a -> LTree a -> Bool
checkNoCover' n = foldTree (\l x r -> if n == x then False else (l || r)) (\x -> n == x)

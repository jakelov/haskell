-- Jake Lovingood Haskell Homework 3 (High Order Functions)
-- 9/8/20
-- Part One: using High-Order Functions

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

-- Use zipWith to implement appendEach and diff function.
appendEach = zipWith (++)
diff = zipWith (-)

-- Use map to implement appendEach
measure x = map (length) x 
bang = map (++ "!") 

-- Using filter to implement digitsOnly and removeXs
digitsOnly y = filter (\x -> 9 > x && x > 0) y
removeXs y = filter (\x -> x == "" || head(x) /= 'X') y

-- Part Two: using fold
-- Part A FindNum
findNum :: Integer -> [Integer] -> Bool
findNum n [] = False
findNum n (x:xs) = (if x==n then True else findNum n xs)
-- Part B findNum
findNum' :: Integer -> [Integer] -> Bool
findNum' n = foldr (\ x y -> if x==n then True else y) False

-- Part A exists
exists :: (a -> Bool) -> [a] -> Bool
exists a [] = False
exists a (x:xs) = (if (a x== True) then True else exists a xs)
-- Part B exists
exists' a = foldr (\ x y -> if (a x==True) then True else y) False

-- Part A noDups
noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = x:noDups (filter (/=x) xs)
-- Part B noDups
noDups' :: Eq a => [a] -> [a]
noDups' = foldr (\x xs -> x : filter (/= x) xs) []

-- Part A countOccurs
countOccurs :: Eq a => a -> [a] -> Integer
countOccurs a [] = 0
countOccurs a (x:xs) | (a==x) = 1+ (countOccurs a xs)
                     | otherwise = countOccurs a xs
-- Part B countOccurs
countOccurs' :: Eq a => a -> [a] -> Integer
countOccurs' n = foldr (\x y -> if (x==n) then 1 + y else y) 0

-- Part A concatList
concatList :: [[a]] -> [a]
concatList [[]] = []
concatList [] = []
concatList (x:xs) = x ++ concatList xs

-- Part B concatList
concatList' :: [[a]] -> [a]
concatList' = foldr (++) [] 

-- Part A bindList 
bindList :: (a -> [b]) -> [a] -> [b]
bindList f [] = [] 
bindList f xs = concatList (map f xs)

-- Part B bindList
bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f = foldr ((++).f)[]

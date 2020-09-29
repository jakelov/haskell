-- Jake Lovingood Recursion PDF Answers
-- DisplayMonth Function, takes an integer and returns a month if valid.
displayMonth :: Integer -> String
displayMonth 1 = "January"
displayMonth 2 = "Feburary"
displayMonth 3 = "March"
displayMonth 4 = "April"
displayMonth 5 = "May"
displayMonth 6 = "June"
displayMonth 7 = "July"
displayMonth 8 = "August"
displayMonth 9 = "September"
displayMonth 10 = "October"
displayMonth 11 = "November"
displayMonth 12 = "December"
displayMonth a = "Error"

-- Function Sign, takes an Integer and returns a String based of if the Int is pos/negative/zero
sign :: Integer -> String
sign a
    | a > 0     = "Positive"
    | a == 0    = "Zero"
    | otherwise = "Negative"

-- AltSum function, computes alternating sum of all Integers up to input number.
altSum :: Integer -> Integer
altSum 0 = 0
altSum 1 = 1
altSum n
    | even n = altSum(n-1) - n
    | odd n = altSum(n-1) + n

-- sumOdds function, Integer where sumOdds n returns the sum of first n odd integers
sumOdds :: Integer -> Integer
sumOdds n
    | n == 0 = 0
    | even n = (n-1)
    | odd n = sumOdds(n-1) + n

-- f, f n computes number fn satisifing n + 2fn-1
f :: Integer -> Integer
f 0 = 1
f n | n == n = n + 2 * (f (n-1))

-- g computes number g n satisifying g 0 = 2, g 1 = 1, g n = 2gn -1 - 3gn -2 
g :: Integer -> Integer
g 0 = 2
g 1 = 1
g n | n == n = 2 * (g (n-1)) - 3 * (g (n-2))

-- dup :: [a] -> [a] Takes a list and returns the list where every eleemnt is listed.
dup :: [a] -> [a] 
dup xs = foldr dup [] xs
    where dup x y = x : x : y

-- insertSpace :: String -> String adds one space between all adjacent characters in string.
insertSpaces :: String -> String
insertSpaces [] = []
insertSpaces (x:xs) = if (length xs == 1) then (x:' ':xs) 
    else x:' ':(insertSpaces xs)

-- pairUp takes list and returns list of pairs where first component is of each pair is zero.
pairUp :: [a] -> [(Integer, a)]
pairUp xs = zip list xs
    where list = replicate (length xs) 0

-- addPairs takes list of pairs of integers and adds elem of each pair
addPairs :: [(Integer, Integer)] -> [Integer]
addPairs [] = []
addPairs ((a,b):xs) = (a+b) : addPairs xs

-- SET TWO
-- countInt n 1 returns num times an integer n occurs in a list
countInt :: Integer -> [Integer] -> Integer
countInt n (x:xs)
    | n == x = 1 + (countInt n xs)
    | length xs == 0 = 0
    | otherwise = countInt n xs

--countEq :: Eq a => a -> [a] -> Integer
--countEq x list = countInt x list


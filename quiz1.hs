-- Question 1
-- Jake Lovingood
f :: Integer -> Integer 
f 0 = 0
f n 
    | n > 0     = n - 2 * f(n-1)
    | otherwise =  0

-- Question 2
swap :: (a,b) -> (b,a)
swap (x,y) = (y, x)

-- Question 3
findZero :: [Integer] -> Bool
findZero [] = False
findZero (n:xs)
    | n == 0 = True
    | otherwise = findZero xs

-- Question 4
countZeros :: [Integer] -> Integer
countZeros [] = 0
countZeros (x:xs)
    | x == 0 = 1 + countZeros xs
    | otherwise = countZeros xs

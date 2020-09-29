-- Jake Lovingood --
-- Question1 --
function1 x = (2 * x) - 1

-- Question2 --
hw x = if (x >= 0) then "nonnegative" else "negative"

-- Question3 --
rep s = s++s

-- Question4 --
function4 = [x | x <- [1..20], x `mod` 2 == 1]

-- Question5 --
way1 = print [2..9]
way2 = tail [1..9]
way3 = take 8 [2..9]

-- Question6 --
oddsOnly xs = [x | x <- xs, odd x]

-- Question7 --
pairUp x = zip [1..x] [1..x] 


length' :: [Int] -> Int
length' xs
  | xs == [] 	= 0
  | otherwise	= length' (tail xs) + 1


length2' :: [Int] -> Int
length2' xs = sum[1 | _ <- xs]

pairs :: [Int] -> [(Int, Int)]
pairs xs = zip xs (tail xs)

isSorted :: [Int] -> Bool
isSorted xs = and  [a <= b | (a,b) <- (pairs xs)] 

-- (p:ps) [3,4,7]  3:4:7:[] (3:ps) (p:5:ps) [] (x:xs)
-- [2] 2:[] (x:xs)
-- [2,3] 2:3:[] (x:xs)
-- [2,3] (x:y:z)
-- [2] (x:y:x) no match
-- pattern matching to define recursive list functions
-- [] (x:xs)

sum' :: [Int] -> Int
sum' []		= 0
sum' (x:xs)	= x + sum' xs

length3' :: [Int] -> Int
length3' [] = 0
length3' (x:xs) = length3' (xs) + 1

selectEven :: [Int] -> [Int]
selectEven [] = []
selectEven (x:xs)
  | mod x 2 == 0	= x : selectEven xs 
  | otherwise	 	= selectEven xs

-- Insertion Sort
-- [1,9,4,7]

-- ins 5 [4,7,9]
-- try to trace iSort [5,4,7,9]
ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
  | x > y	= y : (ins x ys)
  | x <= y	=  x : y : ys

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

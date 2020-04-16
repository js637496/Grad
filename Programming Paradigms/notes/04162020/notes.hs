doubleAll :: [Int] -> [Int]

doubleAll xs = [2 * x | x <- xs]

doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

double :: Int -> Int
double x = x * 2

triple :: Int -> Int
triple x = x * 3

mapInteger :: (Int -> Int) -> [Int] -> [Int]
mapInteger _ [] = []
mapInteger f (x:xs) = f x : mapInteger f xs

even :: Int -> Bool
even x = mod x 2 == 0



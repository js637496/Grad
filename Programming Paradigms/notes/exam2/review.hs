test :: Int -> Int
test n = n


test2 :: [Int] -> [Int]
test2 (x:4:xs) = (x:4:xs)

test3 :: [Int] -> [Int]
test3 (x:y:xs) = (x:y:xs)


test4 :: [Int] -> [Int]
test4 [] = []
test4 [x, _] = [x]
test4 (x:xs) = 1: (x:xs)

secondToLast :: [Int] -> Int
secondToLast [] = error "no second to last item"
secondToLast [x] = error "no second to last item"
secondToLast [x, _] = x
secondToLast (x:xs) = secondToLast(xs)


type Single = Int
type Pair a = (a,a)
type Triple a b = (a,Int,b)

ex1 :: Single
ex1 = 5

ex5 :: Pair Double
ex5 = (5,5)

ex6 :: Triple Int Int
ex6 = (4,7,6)

ex7 :: Triple Double Double
ex7 = (44.4, 16, 33.3)

ex11 :: Triple Int Double
ex11 = (66, 23, 32.4)

ex12 :: Triple Double Int
ex12 = (22.3, length "WCU", 3-2)

sumLst :: [Int] -> Int
sumLst lst = case lst of
             [] -> 0
             (x:xs) -> (x + (sumLst xs))

-- test
sumPosLst :: [Int] -> Int
sumPosLst lst = case lst of
                  [] -> 0
                  (x:xs) -> if (x > 0) then (x + (sumPosLst xs)) else (sumPosLst xs)              


data Set = Set [Int]
           | EmptySet

selectSingleDigits :: Set -> [Int]
selectSingleDigits (EmptySet) = []
selectSingleDigits (Set []) = selectSingleDigits (EmptySet)
selectSingleDigits (Set (x:xs)) = if ((x < 10) && (x > -10)) then x : selectSingleDigits (Set xs) else selectSingleDigits (Set xs)

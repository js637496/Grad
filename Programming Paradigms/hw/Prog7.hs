{- ##################################
   JAMES SPEERS
   04/25/2020
   Homework 7.
   ################################## -}

module Prog7 where

data Expr = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | If BExpr Expr Expr

data BExpr = BoolLit Bool
  | Or BExpr BExpr
  | EqualTo Expr Expr
  | LessThan Expr Expr


-- The bEval function evaluates isntances of BExpr
-- It takes one arguement of type 'BExpr'
-- It ruturns type 'Bool'
bEval :: BExpr -> Bool
bEval (BoolLit b) = b
bEval (Or a b) = (bEval a) || bEval(b)
bEval (EqualTo (x) (y)) = (value x) == (value y)
bEval (LessThan (x) (y)) = (value x) < (value y)  

-- The value function evaulates an expression
-- It takes one argument of type Expr
-- It returns type Int
value :: Expr -> Int
value (Val n) = n
value (Add x y) = (value x) + (value y)
value (Sub x y) = (value x) - (value y)
value (Mul x y) = (value x) * (value y)
value (Div x y) = div (value x) (value y)
value (If b x y) = if (bEval b) then (value x) else (value y)

-- The sumSqNeg function computes the sum of squares of negatives, must use one or more map, filter, foldr
-- It takes one argument of type [Int]
-- It returns type Int
sumSqNeg :: (Num a, Ord a) => [a] -> a
sumSqNeg xs = foldr (+) 0 (filter (<0) xs)

-- The containing function checks if each element in the first list is also in the 2nd list
-- It takes two arguments of type [a] and [a]
-- It returns type Bool
containing :: Eq a => [a] -> [a] -> Bool
containing [] [] = True
containing _ [] = False
containing [] _ = True
containing (x:xs) ys = contains ys x && (containing xs ys)

-- The function total applies a fn to every item in a list and then sums the result
-- It takes two argument a fn of (Int -> Int) and [Int]
-- It returns Int
total :: (Int -> Int) -> [Int] -> Int
total fn xs = sum (map fn xs)

-- Test fn for total
add3 :: Int -> Int
add3 x = x + 3

-- The containing' function checks if each element in the first list is also in the 2nd, using hof
-- It takes two list arguments
-- It returns Bool
containing' :: Eq a => [a] -> [a] -> Bool
containing' (xs) (ys) = filter (contains ys) xs == xs

-- containing helper for filter
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) n = n == x || contains xs n 


-- The lengths function returns a list of lengths for given strings, must use hof
-- It takes one argument of type [String]
-- It returns type [Int]
lengths :: [String] -> [Int]
lengths xs = map (length) xs

-- The max' function returns the largest element of a nonempty list, use hof
-- It takes one list argument
-- It returns an element of the list type
--max' :: Ord a => [a] -> a
max' :: Ord a => [a] -> a
max' [] = error "dont use an empty list"
max' xs = foldr1 (max) xs

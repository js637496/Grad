{- ##################################
   JAMES SPEERS
   02/13/2020
   Homework 2.
   ################################## -}

module Prog2 where

import Data.Char

-- The 'asciisum' recursivley sums the ASCII values in a String.
-- It takes one argument, of type 'String'.
-- It returns type 'Integer'.
asciisum :: String -> Integer
asciisum [] = 0
asciisum (x:xs) = asciisum xs + toInteger (fromEnum x) 

-- The 'integerSqrt' function returns the integer square root of a positive integer.
-- It takes one argument, of type 'Integer'.
-- It returns type 'Integer'.
integerSqrt :: Integer -> Integer
integerSqrt n = floor((fromInteger n) ** (1/2)) 

-- The 'matches' function picks out all instances of an integer n from a list.
-- It takes two arguements of type 'Ineteger' and '[Integer]'.
-- It returns type '[Integer]'.
matches :: Integer -> [Integer] -> [Integer]
matches y xs = [x | x <- xs, x == y]


-- The 'element' function evaluates if an element exists in a list.
-- It takes two arguments of type 'Integer' and '[Integer]'.
-- It returns type 'Bool'.
element :: Integer -> [Integer] -> Bool
element y xs = (matches y xs) /= []

-- The 'elemAt' functtion returns the ith item of a list using recursions.
-- It takes two arguments of type 'Int' and '[Int]'.
-- It returns type 'Int'.
elemAt :: Int -> [Int] -> Int
elemAt y (x:xs)
  | y == 0 	= x
  | otherwise	= elemAt (y-1) xs  

-- The 'and'' function recursivley returns the conjunction of a list of boolean values.
-- It takes one argument of type '[Bool]'.
-- It returns type 'Bool'.
and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = and' bs && b

-- The 'ins'' function is a helper function for insertion sort of a list of (Int,String) pairs.
-- It takes two arguments of type '(Int, String)' and '[(Int, String)]'.
-- It returns type '[(Int, String)]'.
ins' :: (Int, String) -> [(Int,String)] -> [(Int, String)]
ins' (x,sx) [] = [(x,sx)]
ins' (x,sx) ((y,sy):ys)
  | x > y	= (y,sy) : (ins' (x,sx) ys)
  | x <= y	= (x,sx) : (y,sy) : ys

-- The 'iSort'' function sorts a list of (Int, String) pairs using insertion sort.
-- It takes one argument of type '[(Int, String)]'.
-- It returns type '[(Int, String)]'.
iSort' :: [(Int, String)] -> [(Int, String)]
iSort' [] = []
iSort' (x:xs) = ins' x (iSort' xs)

-- The 'upperFirstTwoLetters' function uppcases the first two letters of a String.
-- It takes one argument of type 'String'.
-- It returns type 'String'.
upperFirstTwoLetters :: String -> String
upperFirstTwoLetters [] = ""
upperFirstTwoLetters (x:y:zs) = (toUpper x) : (toUpper y) : zs
upperFirstTwoLetters (x:xs) = (toUpper x) : xs

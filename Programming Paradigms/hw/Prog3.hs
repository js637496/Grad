{- ##################################
   JAMES SPEERS
   02/22/2020
   Homework 3.
   ################################## -}

module Prog3 where

import Data.Char

-- The 'halve' function splits a string with an even length in half into a tupele.
-- It takes one argument, of type 'String'.
-- It returns type '(String, String)'.
halve :: String -> (String, String)
halve s = splitAt (div (length s) 2) s

-- The 'init'' function returns all but the last item of a list
-- It takes one arguement, of type '[Int]'
-- It returns type '[Int]'
init' :: [Int] -> [Int]
init' [] = error "empty list"
init' (x:[]) = []
init' (x:xs) = x : (init' xs) 

-- The 'numTimes' function returns the number fo times an element occurs in a list
-- It takes two arguments of type 'Int' and '[Int]'
-- It returns type 'Int'
numTimes :: Int -> [Int] -> Int
numTimes _ [] = 0
numTimes n (x:xs) 
  | n == x 	= 1 + (numTimes n xs)
  | otherwise	= numTimes n xs

-- The 'lowerOddLetters' function lowers all odd indexed (index starting at 1)
-- It takes one argument of type 'String'.
-- It returns type 'String'.
lowerOddLetters :: String -> String
lowerOddLetters [] = []
lowerOddLetters (x:xs) = toLower x : lowerEvenHelper xs

-- The 'lowerEvenHelper' function is a mutually recursive helper for the 'lowerOddLetters' function
-- It takes one argument of type 'String'.
-- It returns type 'String'. 
lowerEvenHelper :: String -> String
lowerEvenHelper [] = []
lowerEvenHelper (y:ys) = y : lowerOddLetters ys

-- The 'nestedParens' function takes a string and returns true if the string is zeor or more pairs of parentheses ? what is zero pairs?
-- It takes one arguement of type 'String'.
-- It returns type 'Bool'.
nestedParens :: String -> Bool
nestedParens [] = True
nestedParens (x:[]) = False
nestedParens (x:xs) 
  | x == '(' && last xs == ')'	= nestedParens (init xs)
  | otherwise			= False

-- The 'triads' function gerates a list of integer triples such that x^2 + y^2 = z^2 and x,y,z <= n
-- It takes one argument of type 'Int'.
-- It returns type '[(Int,Int,Int)]'.
triads :: Int -> [(Int,Int,Int)]
triads n = [ (x,y,z) | x <- [0..n] , y <- [0..n] , z <- [0..n], (x * x) + (y * y) == (z * z) ]

-- The 'replicate'' function uses standard haskell functions to replicate a character n times
-- It takes two arguments of type 'Int' and 'Char'.
-- It returns type 'String'.
replicate' :: Int -> Char -> String
replicate' n c = [ c | x <- [1..n] ] 

-- The 'replicate''' function using recursion replicates a character n amount of times
-- It takes two arguments of type 'Int' and 'Char'.
-- It returns type 'String'.
replicate'' :: Int -> Char -> String
replicate'' n c
  | n == 0	= []
  | otherwise 	= c : replicate'' (n-1) c

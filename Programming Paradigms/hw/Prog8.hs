{- ##################################
   JAMES SPEERS
   05/02/2020
   Homework 8.
   ################################## -}

module Prog8 where

-- The all' function returns all elements in a list that satisfy a predicate.
-- It takes two arguments one fn of type (a -> Bool) and [a]
-- It returns type Bool

all' :: (a -> Bool) -> [a] -> Bool
all' fn xs = and (map (fn) xs)

-- Test fn for all' and others
tfn :: Int -> Bool
tfn x = x < 3

-- The takeWhile' function selects elements from a list when they satisfy a predicate.
-- It takes a fn of type (a -> Bool) and [a]
-- It returns type [a]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' fn = filter (fn)

-- The concat' function concatinates a list of lists into a single list.
-- It takes a list of lists [[a]]
-- It returns type [a]
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- The mapNums function takes a list of fns and applies them to the same argument producting a list of results
-- it takes a list of fns [(a -> b)] and a
-- It returns [b]
mapNums :: [(a -> b)] -> a -> [b]
mapNums [] _ = []
mapNums (fn:fns) x = fn x : mapNums fns x  

tfn1 :: Int -> Int
tfn1 x = x + 3
tfn2 :: Int -> Int
tfn2 x = x + 6

-- The multN' function returns a fn that multiplies an arg by n
-- It takes one argument of type Int
-- It returns a fn of type (Int -> Int)
multN' :: Int -> (Int -> Int)
multN' n = (n *)

-- The non digit function determines whether a character is of list [1..9]
-- It takes type Char
-- It returns type Bool
nonDigit :: Char -> Bool
nonDigit = \x -> not (elem x ['1'..'9'])

-- The doubleOp function reverses a list fo strings using  
-- This probably isnt the way this shouldbe done, but it will get the right results
doubleOp :: [String] -> [Int]
doubleOp = \xs -> reverse (map (length) xs)

-- The mapNums' function takes a list of fns and applies them to the same argum$
-- it takes a list of fns [(a -> b)] and a
-- It returns [b]
mapNums' :: [(a -> b)] -> a -> [b]
mapNums' fns x = map ($ x) fns 

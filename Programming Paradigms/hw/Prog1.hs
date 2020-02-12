{- ##################################
   JAMES SPEERS
   02/01/2020
   Homework 1.
   ################################## -}

module Prog1 where

-- |The 'isZeroOrGreater' function evaluates whether an integer is greater than or equal to zero..
-- It takes one argument, of type 'Int'.
-- It returns type 'Bool'.
isZeroOrGreater :: Int -> Bool
isZeroOrGreater n = n >= 0

-- |The 'sphereVolume' function computes the volume of a sphere given it's radius.
-- It takes one argument, of type 'Float'.
-- It returns type 'Float'.
sphereVolume :: Float -> Float
sphereVolume r = (4/3)*3.14*(r^3)

-- |The 'ceilingDecimal' function calculates the ceiling of a float
-- It takes one argument, of type 'Float'.
-- It returns type 'Float'.
ceilingDecimal :: Float -> Float
ceilingDecimal n = fromInteger (ceiling n)

-- |The 'averageThree' function returns the average of 3 integers.
-- It takes three arguments, of type 'Integer'.
-- It returns type 'Float'.
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromInteger (x + y + z) / 3

-- |The 'fourDifferent' function returns true if no 2 of 4 arguments are equal otherwise false
-- It takes four arguments, of type 'Integer'.
-- It returns type 'Bool'.
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z
  | w == x || w == y || w == z	= False
  | x == y || x == z		= False
  | y == z			= False
  | otherwise			= True

-- |The 'sum'' function recursivly computes the sum of 1 to n, where n >= 1
-- It takes one argument, of type 'Integer'.
-- It returns type 'Integer'.
sum' :: Integer -> Integer
sum' n
  | n < 1	= 0 -- Not allowing n < 1
  | n == 1	= 1
  | otherwise	= n + sum' (n-1)

-- |The 'abssum' functions recursivley cumputes the sum of the abs vals from n to m, m is <= n
-- It takes two arguments, of type 'Integer'.
-- It returns type 'Integer'.
abssum :: Integer -> Integer -> Integer
abssum m n
  | m == n	= abs m
  | otherwise 	= abssum m (n-1) + abs n 

-- |The 'exponent'' function recursivley computes the resutls of raising some base number, b to somoe exponent e
-- It takes two arguments, of type 'Integer'.
-- It returns type 'Integer'.
exponent' :: Integer -> Integer -> Integer
exponent' b e
  | e < 0	= 0 -- Not allowing e < 0
  | e == 0	= 1 -- is this right? b^0 = 1 ???
  | otherwise	= exponent' b (e-1) * b

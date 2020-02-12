{- #########################################
   James Speers
   01/28/2020
   Homework 0.
   ######################################### -}

module Prog0 where

isSingleDigit :: Integer -> Bool
isSingleDigit n = n < 10 && n > -10

isVowel :: Char -> Bool
isVowel c
  | c == 'a' || c == 'A'	= True
  | c == 'e' || c == 'E'	= True
  | c == 'i' || c == 'I'	= True
  | c == 'o' || c == 'O'	= True
  | c == 'u' || c == 'U'	= True
  | otherwise 			= False

absIsDivisibleByThree :: Integer -> Bool
absIsDivisibleByThree n = (mod (abs n) 3) == 0

middle :: Integer -> Integer -> Integer -> Integer
middle a b c
  | a <= b && a >= c	= a
  | a <= c && a >= b	= a 
  | b <= a && b >= c	= b
  | b <= c && b >= a	= b
  | otherwise		= c

nor :: Bool -> Bool -> Bool
nor a b = not (a || b)

perimeter :: Integer -> Integer -> Integer
perimeter a b
  | a <= 0 || b <= 0	= 0
  | otherwise		= 2 * (a + b)

letterGrade :: Integer -> String
letterGrade a
  | a >= 93	= "A"
  | a >= 90	= "A-"
  | a >= 87	= "B+"
  | a >= 83	= "B"
  | a >= 80	= "B-"
  | a >= 77	= "C+"
  | a >= 73	= "C"
  | a >= 70	= "C-"
  | otherwise	= "F"

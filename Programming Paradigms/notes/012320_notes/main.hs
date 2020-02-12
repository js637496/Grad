x :: Int
x = 3

y :: Integer
y = 34

c :: Char
c = 'h'

s :: String
s = "hello, world"

b1 :: Bool
b1 = True


--load: main    -- loads file into interpretor
-- Compare and contrast functions with operators
-- + - * / mod div ^ 
-- 2 + 3 plus is infix (middle)
-- mod is prefix mod 17 3 = 2 left over 3 into 17 5 times with 2 left over
-- div is prefix div 17 3 = 5 
-- back quote key makes prefix functions infix `````
-- 17 `div` 3 = 5
-- dash sign is overloaded - minus or negative  -7 * -3 causes error
-- (-7) * (-4) fixes this
-- abs prefix function abs -5 error, abs (-5)
-- negate 99 = -99

-- cant add ints and doubls as variables
-- x + z is error
-- can 4 + 7.6

--Type conversion
--3 is considered polymorphic, can be different types, depends on the context it used in
--Num - super class type - Int, double float are all part of Num
-- cant do x + 4.5 because x is int 4.5 si flot or dub
-- / only works for floats or dub
-- 4.5/2.4 Ok, 4/2.4 Ok, x/2.3 error
-- div is integer division
-- div 16 x Ok, div 16 3.2 error


--Boolean logic
-- && || not
-- == /= < > <= >=
-- True False - cap first letter
-- can compare strings using relation ops "haskel" > "java" ???


--Functions
-- Int -> Int (arrow)
-- Int to Int

addOne :: Int -> Int
addOne n = n + 1

timesThree :: Int -> Int
timesThree n = n * 3

--timesThree (addOne 5)
--18

--Functions with multiple parameters
--Fun to test 3 ints are equal
threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m == n) && (m == p)

-- test where 4 ints are qual

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p q = (m == n) && (n == p) && (p == q)

fourEquals' :: Int -> Int -> Int -> Int -> Bool
fourEquals' a b c d = (threeEqual a b c) && (c == d)

-- if statements in haskell
-- if BOOL then DO THIS else DO THAT
--something is always returned, data type of then part must be dt of then part

-- retrn max of 2 int values
twoMax :: Integer -> Integer -> Integer
twoMax x y = if x >= y then x else y

threeMax :: Integer -> Integer -> Integer -> Integer
threeMax x y z = twoMax z (twoMax x y) 

-- test

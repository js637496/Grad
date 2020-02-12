import Data.Char

threeEqual :: Int -> Int -> Int -> Bool
threeEqual x y z = (x == y) && (y == z)

-- Guards - Else in haskell
-- name x1 x2 ... xn
--  g1  = e1
--  g2  = e2
--  ...
--  gn  = gn
--  otherwise  = e
--Guards are bool

threeEqual' :: Int -> Int -> Int -> Bool
threeEqual' x y z
  | x == y && y == z	= True
  | otherwise		= False

twoMax :: Int -> Int -> Int
twoMax x y
  | x > y	= x
  | otherwise	= y

threeMax :: Int -> Int -> Int -> Int 
threeMax x y z
  | x > y && x > z	= x
  | y > x && y > z	= y
  | otherwise 		= z


-- 1::Int force to be int
-- :type - get type of var
-- :info (+)

--Factorial
-- Base Case 0! = 1
-- fac
fac :: Int -> Int
fac n
  | n == 0	= 1
  | otherwise	= fac (n-1) * n

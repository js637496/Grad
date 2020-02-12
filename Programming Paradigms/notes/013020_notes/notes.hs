--013020
fac :: Int -> Int
fac n
  | n == 0	= 1
  | otherwise 	= fac ((abs n)-1) * (abs n)

-- 0! + 1! + ... + n!

sumfac :: Int -> Int
sumfac n
  | n == 0	= 1
  | n > 0	= fac n + sumfac (n-1)

-- natural numbers 0 and positve summation of nums between n and m

nat :: Int -> Int -> Int
nat n m
  | m == n 		= n
  | otherwise		= n + nat (n+1) m

not' :: Bool -> Bool
not' True = False
not' False = True

f :: Int -> Int
f 0 = 10
f 1 = 11

fac' :: Int -> Int
fac' 0 = 1
fac' n = fac (n-1) * n



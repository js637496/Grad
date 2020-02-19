double :: Int -> Int
double n = 2*n

xor :: Bool -> Bool -> Bool
xor n x = n /= x 

selectEven :: [Int] -> [Int]
selectEven xs = [ x | x <- xs, mod x 2 == 0]

selectEven' :: [Int] -> [Int]
selectEven' [] = []
selectEven' (x:xs)
  | mod x 2 == 0 = x:(selectEven' xs)
  | otherwise	 = selectEven' xs


elemNum :: Int -> [Int] -> [Int]
elemNum z ys = [ x | x <- ys, x == z ] 

elemNum' :: Int -> [Int] -> [Int]
elemNum' _ [] = []
elemNum' z (x:xs)
  | z == x    = z : elemNum' z xs
  | otherwise = elemNum' z xs

reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- mutual recursion
evens :: String -> String
evens [] = []
evens (x:xs) = x : odds xs

odds :: String -> String
odds [] = []
odds (y:ys) = evens ys


-- SCOPE
maxsq :: Int -> Int -> Int
maxsq x y 
  | sqx > sqy = sqx
  | otherwise = sqy
    where
    sqx = sq x
    sqy = sq y
    sq :: Int -> Int
    sq x = x * x

-- Case Expressions

--case e of
--  p1 -> e1
--  p2 -> e2
--  ...
--  pk -> ek

-- case  (something that can be evaluated)    of
test :: String -> Int
test x = case "hello" of
  [] -> 3
  ('h' : xs) -> length xs
  _ -> 7

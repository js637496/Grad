{- ##################################
   JAMES SPEERS
   04/4/2020
   Homework 5.
   ################################## -}

module Prog5 where

data Set = Set [Int]
           | EmptySet
  deriving Show

-- The 'member' function checks where a given item is present in a given set.
-- It takes two arguments of type 'Int' and 'Set'.
-- It returns type 'Bool'
member :: Int -> Set -> Bool
member n (Set []) = member n (EmptySet)
member n (EmptySet) = False
member n (Set (x:xs)) = x == n || (member n (Set xs))

s1 :: Set
s1 = Set [1,2,3,4,5,6]

s2 :: Set
s2 = EmptySet

s3 :: Set
s3 = Set []

s4 :: Set
s4 = Set [1,2,3,4,5,6]

s5 :: Set
s5 = Set [6,3,1,2,4,5]

s6 :: Set
s6 = Set [1,2,3]

s7 :: Set
s7 = Set []

s8 :: Set
s8 = Set [1]

-- The 'ins' function adds an item to a set
-- It takes two arguments of type 'Int' and 'Set'
-- It returns tpye 'Set'
ins :: Int -> Set -> Set
ins n (Set []) = ins n (EmptySet)
ins n (EmptySet) = Set [n]
ins n (Set xs)
  | member n (Set xs) = (Set xs)
  | otherwise 	      = (Set (n : xs))

-- Function to sort the set list so equal check will be easier
-- The 'ins'' function is a helper function for insertion sort of a list of Int.
-- It takes two arguments of type 'Int' and '[Int]'.
-- It returns type '[Int]'.
ins' :: Int -> [Int] -> [Int]
ins' x [] = [x]
ins' x (y:ys)
  | x > y	= y : (ins' x ys)
  | x <= y	= x : y : ys

-- The 'iSort'' function sorts a list of Int using insertion sort.
-- It takes one argument of type '[Int]'.
-- It returns type '[Int]'.
iSort' :: [Int] -> [Int]
iSort' [] = []
iSort' (x:xs) = ins' x (iSort' xs)

-- The 'equal' function tesest whether two sets are equal
-- It takes two arguments of type 'Set' and 'Set'
-- It returns type 'Bool'
equal :: Set -> Set -> Bool
equal (Set []) (Set []) = equal (EmptySet) (EmptySet)
equal (Set x) (Set []) = equal (Set x) (EmptySet)
equal (Set []) (Set x) = equal (EmptySet) (Set x)
equal (EmptySet) (Set []) = equal (EmptySet) (EmptySet)
equal (Set []) (EmptySet) = equal (EmptySet) (EmptySet)
equal EmptySet EmptySet = True
equal _ EmptySet = False
equal EmptySet _ = False
equal (Set x) (Set y) = (iSort' x) == (iSort' y)

--Helper function to convert Set [] to EmptySet
convertToEmptySet :: Set -> Set
convertToEmptySet (EmptySet) = (EmptySet)
convertToEmptySet (Set []) = (EmptySet)
convertToEmptySet (Set n) = (Set n)

-- The 'saferemove' function removes an item form a set
-- It takes two arguments 'Int' and 'Set'
-- It returns type 'Maybe Set'
saferemove :: Int -> Set -> Maybe Set
saferemove n (EmptySet) = Nothing
saferemove n (Set xs)
  | member n (Set xs) == False = Nothing
  | otherwise 		       = Just (convertToEmptySet (Set ([x | x <- xs, x /= n])))

-- The 'countLetters' function takes three strings as input and returns and Int list of the strings lengths
-- It returns type IO [Int]
countLetters :: IO [Int]
countLetters = do line1 <- getLine
                  line2 <- getLine
                  line3 <- getLine
                  return ([(length line1),(length line2),(length line3)])

-- The 'and'' function takes a bool and asks for an input "True" or "False" from user and returns the bool and equalivlent of the two bools
-- It takes one argument of type 'Bool'
-- It returns type 'IO Bool'
and' :: Bool -> IO Bool
and' b = do line1 <- getLine
            return (b && line1 == "True")

data Tree1 = Leaf1 Int
             | Node1 Tree1 Int Tree1

t1 :: Tree1
t1 = Node1 (Leaf1 4) 3 (Node1 (Leaf1 5) 2 (Leaf1 1))

t2 :: Tree1   
t2 = Node1 (Leaf1 4) 3 (Node1 (Leaf1 (-5)) 2 (Leaf1 1))

-- The 'preorder' function takes a Tree and flattens a preorder traversal of the tree into a list
-- It takes one argument of type 'Tree1'
-- It returns type [Int]
preorder :: Tree1 -> [Int]
preorder (Leaf1 x) = [x]
preorder (Node1 left v right) = (preorder right) ++ [v] ++ (preorder left)

-- The 'sumPositives' function sums all postive integers in a Tree1
-- It takes one argument of type 'Tree1'
-- It returns type 'Int'
sumPositives :: Tree1 -> Int
sumPositives t = sum [x | x <- (preorder t), x > 0]

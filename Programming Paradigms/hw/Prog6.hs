{- ##################################
   JAMES SPEERS
   04/18/2020
   Homework 6.
   ################################## -}

module Prog6 where

data Tree a = Leaf a
  	    | Node [Tree a]
  deriving Show

t1 :: Tree Int
t1 = Node [Node [Leaf 1, Leaf 2, Leaf 3], Node [Leaf 4, Leaf 5, Node [Leaf 6]]]

t2 :: Tree Char
t2 = Node [Node [Leaf 'r', Leaf 'k', Leaf 'u'], Node [Leaf 'h', Leaf 'a', Node [Leaf 'a', Leaf 'd', Leaf 'c']]]

t3 :: Tree Int
t3 = Node [Node [Leaf 1, Leaf 2], Node [Leaf 3, Node [Node [Leaf 4, Leaf 5], Leaf 6]]]
-- The 'occurs' function returns where a given arugment is present in a tree as a leaf
-- It takes two arguments of type a and type Tree a
-- it returns type 'Bool'
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf v) = x == v
occurs x (Node []) = False
occurs x (Node (v:vs)) = occurs x (v) || occurs x (Node vs)

-- The 'countLeaves' function returns the number of leaves in a Tree
-- It takes one argument of type 'Tree a'
-- It returns type 'Int'
countLeaves :: Tree a -> Int
countLeaves (Leaf v) = 1
countLeaves (Node []) = 0
countLeaves (Node (v:vs)) = countLeaves (v) + countLeaves (Node vs)

t8 :: Tree Int
t8 = Node [Leaf 1, Leaf 2, Leaf 3]

t4 :: Tree Int
t4 = Node [Leaf 1, Leaf 2]

t5 :: Tree Int
t5 = Node []

tt :: Tree Int 
tt = Node [Node [Leaf 1, Leaf 2], Leaf 3]



t12:: Tree Int

t12 = Node[Node [Leaf 4, Leaf 5], Node [Node [Leaf 1, Leaf 2], Node [Leaf 3, Leaf 7]]]

t9 :: Tree Int
t9 = Node [Leaf 1, Node []]

t6 :: Tree Int
t6 = Node [Leaf 1]

t7 :: Tree Int
t7 = Leaf 6

t11 :: Tree Int
t11 = Node [Node [Leaf 1], Leaf 2, Node [Leaf 3, Node [Leaf 4]]]
-- Ask if a Tree will ever just be a single Leaf

-- The 'isBinary' function returns bool value if the Tree is binary - each branch has 2 leafs
-- It takes one argument of type 'Tree a'
-- It returns type 'Bool'
isBinary :: Tree a -> Bool
isBinary (Leaf _) = True
isBinary (Node []) = False
isBinary (Node (x:y:zs)) = length (x:y:zs) == 2 && isBinary (x) && isBinary (y)
isBinary (Node (x:xs)) = False
-- The 'pre' function returns the preorder traversal of thenodes in a tree\
-- it Takes a Tree a and returns [a]
pre :: Tree a -> [a]
pre (Leaf v) = [v]
pre (Node []) = []
pre (Node (x:xs)) = pre (x) ++ pre (Node xs) 


--depthK returns all leaf values at depth, leafs after root node are 0
--it returns [a]
depthK :: Int -> Tree a -> [a]
depthK n (Leaf x) = if n == (0) then [x] else []
depthK n (Node []) = []
depthK n (Node (x:xs)) = depthK (n-1) (x) ++ depthK (n) (Node (xs))


data Expr= Val Int
             | Add Expr Expr
             | Sub Expr Expr
             | Mul Expr Expr
             | Div Expr Expr

value1 :: Expr -> Int
value1 (Val x) = x
value1 (Add x y) = (value1 x) + (value1 y)
value1 (Sub x y) = (value1 x) - (value1 y)
value1 (Mul x y) = (value1 x) * (value1 y)
value1 (Div x y) = div (value1 x) (value1 y)

value2 :: Expr -> Maybe Int
value2 (Val x) = Just x
value2 (Add x y) = Just ((value1 x) + (value1 y))
value2 (Sub x y) = Just ((value1 x) - (value1 y))
value2 (Mul x y) = Just ((value1 x) * (value1 y))
value2 (Div x y) 
  | (value1 y) == 0 	= Nothing
  | otherwise 	= Just (div (value1 x) (value1 y))

instance Show Expr where
  show (Val x) = show x
  show (Add x y) = "(" ++ show (x) ++ "+" ++ show (y) ++ ")"
  show (Sub x y) = "(" ++ show (x) ++ "-" ++ show (y) ++ ")"
  show (Mul x y) = "(" ++ show (x) ++ "*" ++ show (y) ++ ")"
  show (Div x y) = "(" ++ show (x) ++ "/" ++ show (y) ++ ")"




data Expr = Lit Int
            | Add Expr Expr
            | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

show' :: Expr -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ (show' e1) ++ "+" ++ (show' e2) ++ ")"
show' (Sub e1 e2) = "(" ++ (show' e1) ++ "-" ++ (show' e2) ++ ")"

e1 :: Expr
e1 = Lit 123

e2 :: Expr
e2 = Add (Lit 2) (Lit 3)

e3 :: Expr
e3 = Add (Sub (Lit 3) (Lit 1)) (Lit 3)

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
  deriving Show

occurs :: Eq a => a -> Tree a -> Bool
oocurs x (Leaf v) = x == v
occurs x (Node left v right) = x == v || (occurs x left) || (occurs x right)

-- perform inorder traversal and flatten tree into list
--         3
--       4   5
--         2   1
--  43251

t1 :: Tree Int
t1 = Node (Leaf 4) 3 (Node (Leaf 5) 2 (Leaf 1))

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node left v right) = (flatten left) ++ [v] ++ (flatten right)
--t :: Node
--t =  Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

--   1
-- x   2
--   y   z

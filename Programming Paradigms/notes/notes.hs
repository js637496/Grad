data Tree a = Leaf a
              | Node [Tree a]

t1, c2, c4 :: Tree Int

t1 = Node [ Leaf 3 , c2 , Leaf 5 , c4 ]

c2 = Node [ Leaf 7, Leaf 8 ]
c4 = Node [ Leaf 9, Leaf 1 ]

-- length is polymorphic
-- is there a single def, but ti can take multiple tyes? then its polymophic -- a


--Overloaded 
-- need different defintions for different types


-- :info show
-- Class Show a where
--   show :: a -> String

-- :info (+)
-- Class Num a where
--  (+) :: a -> a -> a

-- Class , Instances

elem' :: a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = (x == y) || elem' x ys

-- Works on multiple types, is polymorphic

-- elemBool :: Bool -> [Bool] -> Bool
-- elemInt ....


-- ad hoc polymorphism - multple types allowed but not every type
--  A Class in haskell is different than in Java
-- A class is more similar to an interface
-- Defines a set of operations
class eq'' a where
  (equall) :: a -> a -> Bool
  (notequall) :: a -> a -> Bool

elem :: eq'' a => a -> [a] -> Bool

data Shape = Circle Int | Rect Int Int | Square Int
instance Eq Share where
  (Circle r1) == (Circle r2) = r1 == r2
  (Rect l1 w1) == (Rect l2 w2) = l1 == l2 && w1 == w2
  (Square s1) == (Square s2) = s1 == s2
  (Rect l w) == (Square s) = l == w && l == s
  (Square s) == (Rect l w) = l == w && l == s
  _ == _ = False
--  shape1 /= shape2 = not (shape1 == shape2)
-- Default implementation /= reuse ==

data Roman = Roman Int
instance Show Roman where
  show (Roman n)
    | n <= 0 = "non Roman numeral"
    | n >= 1 && n <= 3 = replicae n 'I'
    | n == 4 = "IV"
    | n == 5 = "V"
    | n >= 6 && n <= 8 = "V" ++ replicate (n-5) 'I'
    | n == 9 = "IX"
    | n == 10 = "X"

-- why wouldnt deriving show work
-- r :: Roman
-- r = Roman 3
-- ghci
-- > r
-- Roman 3

-- we want III

class Listable a where
  toList :: a -> [Int]

intstance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList xs = xs

instance Listable Tree where
  toList Empty = []
  toList (Node x left right) = toList left ++ [x] ++ toList right


SumL xs = sum (toList xs)

{-# LANGUAGE FlexibleInstances #-}

type Shopitem = (String, Int)
type Basket = [Shopitem]

addCartItems :: Basket -> Int
addCartItems [] = 0
addCartItems ((item,price):xs) = price + addCartItems xs

type Map k v = [(k,v)]
m1 :: Map String Int
m1 = [("aa", 1), ("abx", 1)

m2 :: Map Char Double
m2 = [('a',5.7),('x',53.2)]

find :: k -> Map k v -> v
find k m = head[v | (k',v) <- m, k == k']

type Pos = (Int,Int)
data Move = North | South | East | West
move :: Move -> Pos -> Pos
move North (x,y) = (x, y + 1)
move West (x,y) = (x - 1, y)
move South (x,y) = (x, y - 1)
move East (x,y) = (x + 1, y)

moves :: Pos -> [Move] -> Pos
moves p [] = p
moves p (m:ms) = moves ( move m p) ms

data Shape = Circle Float
	   | Rect Float Float

shape1 :: Shape 
-- t :: Bool
-- t :: False no
shape1 = Rect 1.4 2.4
-- t = True

--Using alegbraic data type as funtction
createSquare :: Float -> Shape
createSquare s = Rect s s

-- takes a data type
area :: Shape -> Float
area (Circle r = 3.14 * r * r
area (Rect l w) = l * w

isRound :: Shape -> Bool
isRound (Circle _) = True 
isRound (Rect _ _) = False

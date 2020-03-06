{- ##################################
   JAMES SPEERS
   03/1/2020
   Homework 4.
   ################################## -}

module Prog4 where

-- Shopping cart:  Items have Price, Bar Code and Name (Item).
type Item = String
type Price = Int
type BarCode = Int
type ShoppingCart = [BarCode]
type Database = [(BarCode, Item, Price)]

-- db1 is of type Database
db1 :: Database
db1 = [ (1234, "Hot Dog", 2),(4719, "Fries", 1),(3814, "Soda", 1),(1112, "Chips", 1),(1113, "Apple", 1) ]
badDB :: Database
badDB = [ (1234, "H", 2),(4232, "F", 1),(1234, "D", 1) ]
--noDB :: Database
--noDB = []

-- cart1 is of type ShoppingCart
cart1 :: ShoppingCart
cart1 = [1234, 4719, 3814, 1112, 1113, 1234]
cart2 :: ShoppingCart
cart2 = [1234,1234,1112,1112,1113,4719]
badcart :: ShoppingCart
badcart = [1234, 4719, 3814, 5555]

-- The 'isValidBarCode' function checks where a barcode is present in a store's database
-- It takes two arguements of type 'BarCode' and 'Database'.
-- It returns type 'Bool'.
isValidBarCode :: BarCode -> Database -> Bool
isValidBarCode bc db = or [ x == bc | (x,_,_) <- db, x == bc ]

-- The 'isValidDatabase' function checks whether each barcode in the databse is unique
-- It takes one argument of type 'Database'.
-- It returns type 'Bool'.
isValidDatabase :: Database -> Bool
isValidDatabase [] = True
isValidDatabase db = and (isValidDBHelper db)

-- The 'isValidDBHelper' function creates a list where if each bar code appears once is True otherwise False
-- It takes one arguement of type 'Database'.
-- It returns type '[Bool]'.
isValidDBHelper :: Database -> [Bool]
isValidDBHelper [] = [True]
isValidDBHelper ((z,_,_):xs) = (sum ([ 1 | (y,_,_) <- xs, z == y]) == 0) : (isValidDBHelper xs) 

-- The 'totalBill' function takes a shopping car and totals the cost of the items inside.
-- It takes two arguments of type 'ShoppingCart' and 'Database'
-- It returns type 'Int'
totalBill :: ShoppingCart -> Database -> Int
totalBill [] _  = 0
totalBill s db
  | not (isValidDatabase db) = error "Invalid database"
  | not (and [ isValidBarCode bc db | bc <- s ]) = error "One or more items in cart is invalid"
  | otherwise = sum [ p | (bc,_,p) <- db, sbc <- s, sbc == bc ]  

getPrice :: BarCode -> Database -> Int
getPrice bc db = sum ([ p | (b,_,p) <- db, bc == b ])

-- The 'duplicateItems' function returns the set of items that appear more than once in a ShoppingCart.
-- It takes two arguments of type 'ShoppingCart' and 'Database'.
-- It returns type '[String]'.
duplicateItems :: ShoppingCart -> Database -> [String]
duplicateItems [] _ = []
duplicateItems (x:xs) db
  | (getCount x (x:xs)) > 1 = (getItem x db) : duplicateItems xs db
  | otherwise 		  = duplicateItems xs db

-- Get count of item in shopping cart
getCount ::  BarCode -> ShoppingCart -> Int
getCount bc sc = sum [1 | x <- sc, bc == x]

-- The 'getItem' function returns name of item from database.
-- IT takes two arguments of type BarCode and Database
-- it returns type String
getItem :: BarCode -> Database -> String
getItem _ [] = error "Item not found in database"
getItem bc ((x,y,_):xs)
  | bc == x 	= y
  | otherwise   = getItem bc xs
--Changes 3/2 - NOPE Maybe?
--data LicensePlate = IntPlate Int Int
--		    | CharPlate [Char] Int

type LicensePlate a = ([a], [Int])
lp1 :: LicensePlate Int
lp1 = ([1,2,3],[4,5,6,7])
lp2 :: LicensePlate Char
lp2 = ("ABC",[4,5,6,7])
lp3 :: LicensePlate Char
lp3 = ("ABC",[1,2,3,4,5])
lp4 ::  LicensePlate Char
lp4 = ("ABCD",[1,2,3,4])

--Changes 3/2 - NOPE Maybe?
--InvalidPlate :: LicensePlate -> Bool


-- The isValidPlate function returns true of 3 items in first part followed by 4 items in 2nd part/
-- It takes one arg type LicensePlate
-- It returns Bool
isValidPlate :: LicensePlate a -> Bool
isValidPlate (x, y)
  | (length x) == 3 && (length y) == 4  = True
  | otherwise				= False

-- The firstPartPlate function returns the first part of the plate
-- It takes one arg LicesnePlate
-- it returns type [a]
firstPartPlate :: LicensePlate a -> [a]
firstPartPlate (x, y) = x

-- The sumFirstPartPlate function sums the first part of the plate
-- it takes tpye lisenceplate
-- it returns int 
-- gotta do some type of checks here
--sumFirstPartPlate :: LicensePlate a -> Int
--sumFirstPartPlate lp = sum (firstPartPlate lp)

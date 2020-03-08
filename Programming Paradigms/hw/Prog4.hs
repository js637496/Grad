git{- ##################################
   JAMES SPEERS
   03/1/2020
   Homework 4.
   ################################## -}

module Prog4 where

import Data.Char

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


data LicensePlate = IntPlate Int Int
                  | CharPlate [Char] Int

  deriving Show

lp1 :: LicensePlate
lp1 = IntPlate 123 4567

lp2 :: LicensePlate
lp2 = CharPlate "abc" 4567

testlp1 :: LicensePlate
testlp1 = IntPlate 001 4563 -- this is always going to be represented as 1 never 001, unless we use String
testlp2 :: LicensePlate
testlp2 = IntPlate 12 4234
testlp3 :: LicensePlate
testlp3 = IntPlate 123 123
testlp4 :: LicensePlate
testlp4 = CharPlate "ba" 1234
testlp5 :: LicensePlate
testlp5 = IntPlate 123 9999
testlp6 :: LicensePlate
testlp6 = CharPlate "edf" 9999

-- The 'isValidPlate' function takes a LicensePlate and returns wheter it is valid.
-- It takes one argument of type 'LicensePlate'
-- It returns type 'Bool'
isValidPlate :: LicensePlate -> Bool
isValidPlate (IntPlate x y) = length (show x) == 3 && length (show y) == 4
isValidPlate (CharPlate x y) = length x == 3 && length (show y) == 4

-- The 'nextPlate' function takes a 'LicensePlate' and adds one to the 2nd half resetting to 1000 if 9999
-- It takes one argument of type 'LicensePlate'
-- It returns type 'LicensePlate'
nextPlate :: LicensePlate -> LicensePlate
nextPlate (IntPlate x y) 
  | y == 9999 = IntPlate x 1000
  | otherwise = IntPlate x (y+1)
nextPlate (CharPlate x y)
  | y == 9999 = CharPlate x 1000
  | otherwise = CharPlate x (y+1)

-- The 'sumFirstPartPlate' function takes a license plater and sumes the first part, Char are converetd to ascii int values
-- It takes one argument of type 'LicensePlate'
-- It returns type 'Int'
sumFirstPartPlate :: LicensePlate -> Int
sumFirstPartPlate (IntPlate x y)  = sum [ digitToInt n | n <- (show x) ]
sumFirstPartPlate (CharPlate x y) = sum [ fromEnum n | n <- x ]

-- The 'showPlate' function takes a 'LicensePlate' and returns the string equivilent with a - in the middle
-- It takes one argument of type 'LicensePlate'
-- It returns type 'String'
showPlate :: LicensePlate -> String
showPlate (IntPlate x y) = (show x) ++ "-" ++ (show y)
showPlate (CharPlate x y) = x ++ "-" ++ (show y)

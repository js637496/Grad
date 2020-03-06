data Person = Person String Int
-- type constructor  ^ data constructor
-- new data type called person

student1 :: Person
student1 = Person "Tom" 42

getAge :: Person -> Int
getAge (Person n a) = a
-- returns 42 when student1 passed in

--data Maybe a = Nothing
--             | Just a

-- > head [1,2,3]
-- head [] 
-- error
-- 5 `div` 0
-- error
-- head :: [a] -> a
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safedive x y = Just (div x y)

convert :: Maybe Int -> Int
convert (Nothing) = 0
convert (Just x) = x

--inputInt :: Int

--inputDiff :: Int
--inputDiff = inputInt - InputInt


-- do a1
--    v2 <- a2
--    v3 <- a3
--    a4
--    ...
--    vn <- an
--    return (f v2 v3 .. vn)

act :: IO (Char, Char)
act = do x <- getChar
      getChar
      y <- getChar
      return (x,y)


putStr :: String -> IO ()
putStr (x:xs) = do putChar x
                   putStr xs
putStr [] = return ()

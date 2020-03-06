--to string in haskell
--show :: Show a => a-> String
-- (show x)

-- read "123"::Int
-- (read "123")::Int

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x 
		    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' str = do putStr' str
		   putStr' "\n"

getLine' :: IO String
getLine' = do x <- getChar
	      if x == '\n' then return []
	      else 
		   do xs <- getLine'
                      return (x:xs)

getInt' :: IO Int
getInt' = do line <- getLine'
             return ((read line)::Int)

triads :: Int -> [(Int,Int,Int)]
triads n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x*x) + (y*y) == (z*z) ]

act1 :: IO ()
act1 = do putStr' "Enter a number: "
          num <- getInt'
          putStrLn' (show (triads num))

act2 :: IO ()
act2 = do putStr' "Enter a number: "
          num <- getInt'
          let result = triads num
          putStrLn' (show result)
       -- result <- triads num

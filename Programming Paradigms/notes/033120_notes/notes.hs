--- inputInt

readThreeChars :: IO (Char, Char)
readThreeChars = do x <- getChar
		    getChar
		    y <- getChar
		    return (x,y)

copy :: IO ()

copy = do line <- getLine
          putStrLn line
          copy

copyn :: Int -> IO ()
copyn n = if n <= 0
          then return ()
          else do line <- getLine
                  putStrLn line
                  copyn (n-1)

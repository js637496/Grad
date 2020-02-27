digits :: String -> String
digits st = [ c | c <- st, c >= '0' && c <= '9' ]

firstDigit :: String -> Char
firstDigit st = case (digits st) of
			(x:_) -> x
			[] -> '\0'  
firstDigit' st
  | null (digits st) = '\0'
  | otherwise 	     = head(digits st)

head' :: [Int] -> Int
head' [] = error "empty list"
head' (x:_) = x

head'' :: [Int] -> Int
head'' xs = case xs of
		[] -> error "empty list"
		(x:_) -> x

head''' :: [a] -> a
head''' [] = error "emtyp lsit"
head''' (x:_) = x



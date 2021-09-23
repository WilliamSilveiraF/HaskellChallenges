zerocem :: [Int] -> [Int]
zerocem myNum = [ x | x <- myNum, between x]
     where
         between x = 0 <= x && x <= 100
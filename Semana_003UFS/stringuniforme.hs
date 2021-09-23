import Data.Char

teste :: String -> String
teste st = [paraMaiusculo x | x <- st]
    where
     paraMaiusculo c
      | ehMinusculo c = chr (ord c - ord 'a' + ord 'A')
      | otherwise = c
       where 
           ehMinusculo c = ('a' <= c) && (c <= 'z')
import Data.Char

ehMinusculo :: Char -> Bool
ehMinusculo c = ('a' <= c) && (c <= 'z')

paraMaiusculo :: Char -> Char
paraMaiusculo c
     | ehMinusculo c = chr (ord c - ord 'a' + ord 'A')
     | otherwise = c
import Data.Char

ordem :: Char -> Char -> Char
ordem a b = if (toUpper a) > (toUpper b)
        then b
    else if (toUpper b) > (toUpper a)
        then a
    else '0'
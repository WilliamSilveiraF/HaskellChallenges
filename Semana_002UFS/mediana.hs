mediana a b c
    | (b < a && a < c) || (c < a && a < b) = a
    | (a < b && b < c) || (c < b && b < a) = b
    | (a < c && c < b) || (b < c && c < a) = c
    | (a == b) || (a == c) || (b == c) = media
        where
            media = (a + b + c) `div` 3
diferente :: Int -> Int -> Int -> Bool
diferente x y z = x /= y && x /= z && y /= z

maior :: Int -> Int -> Int
maior x y
    | x > y = x
    | otherwise = y

condmaior :: Int -> Int -> Int
condmaior x y =
    if x > y
        then x
        else y
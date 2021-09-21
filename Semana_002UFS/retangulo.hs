areart :: Int -> Int -> Int
areart a b = a * b

-- area do rentagulo
teste a b
    | a == b = 0
    | otherwise = areart
    where
        areart = (a * b)

somaOrdPares :: [(Int, Int)] -> [Int]
somaOrdPares lista = [x + y | (x, y) <- lista, x < y]


checkbox :: [(Int, Int)] -> [(Int, String)]
checkbox paramentro = [(farea x y, ftipo x y) | (x, y) <- paramentro]
    where
        farea x y = x * y
        ftipo x y
           | x == 0 && y == 0 = "um nada"
           | x == 0 || y == 0 = "uma reta"
           | x == y           = "quadrado"
           | otherwise        = "retangulo"


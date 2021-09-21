souPar :: Int -> Int
souPar variavel
     | mod variavel 2 == 0  = 1
     | otherwise            = 0

contadorPar :: Int -> Int -> Int -> Int -> Int
contadorPar a b c d = (souPar a) + (souPar b) + (souPar c) + (souPar d)
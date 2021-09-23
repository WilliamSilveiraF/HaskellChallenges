triplo :: [Int] -> [Int]
triplo myList = [3*x | x <- myList, (testeImpar x == True)]
    where 
        testeImpar x = mod x 2 /= 0
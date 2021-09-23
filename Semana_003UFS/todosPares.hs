todosPares :: [Int] -> Bool
todosPares xs = (xs == [x | x <- xs, ehPar x])
   where
       ehPar x = (mod x 2 == 0)


todosImpares :: [Int] -> Bool 
todosImpares lista = (lista == [x | x <- lista, ehImpar x])
   where
       ehImpar x = (mod x 2 /= 0)
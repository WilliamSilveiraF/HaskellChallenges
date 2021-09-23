comTupla :: [(Int, Int, Int)] -> [(Bool, Bool, Bool)]
comTupla lista = [(ehPar x, ehPar y, ehPar z) | (x, y, z) <- lista]
   where
       ehPar n =  (n `mod` 2 == 0)


semTupla :: [Int] -> [Bool]
semTupla lista = [ehImpar n | n <- lista]
   where
       ehImpar n =  (n `mod` 2 == 0)

--note que n>50 tem efeito exclusivo
stuplaComTestes :: [Int] -> [Bool]
stuplaComTestes lista = [mult6 n | n <- lista, n > 50]
   where
       mult6 n = (n `mod` 6 == 0)

--note que n > 50 tem efeito booleano
stuplaCPam :: [Int] -> [Bool]
stuplaCPam lista = [mult6 n && n > 50| n <- lista]
   where
       mult6 n = (n `mod` 6 == 0)


somaPares :: [(Int,Int)] -> [Int]
somaPares lista = [x+y| (x,y)<-lista]






--Escreva um função que dados dois números inteiros, 
--retorna o maior e o menor  valores

maiorMenor :: Int -> Int -> (Int, Int)
maiorMenor a b
   | a >= b    = (a, b)
   | otherwise = (b, a)


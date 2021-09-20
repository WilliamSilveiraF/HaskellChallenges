mediageral :: Int -> Int -> Int -> Int
mediageral x y z = (x + y + z) `div` 3

soumaior :: Int -> Int -> Int
soumaior variavel media =
    if variavel > media
         then 1
         else 0

contadorAcima :: Int -> Int -> Int -> Int
contadorAcima x y z = (soumaior x mediagrl) + (soumaior y mediagrl) + (soumaior z mediagrl)
    where 
        mediagrl = mediageral x y z
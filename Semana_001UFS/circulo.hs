area :: Float -> Float
area raio = raio*raio*pi

perimetro :: Float -> Float -> Float
perimetro comp raio = 2*comp*raio

--diferenca de areas de um circulo
diferenca :: Float -> Float -> Float
diferenca r1 r2 = abs ((area r1) - (area r2))
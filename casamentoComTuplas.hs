--Casamento Padrão em Tuplas
somaCord :: (Int, Int) -> Int
somaCord (x, y) = x + y

nome :: (String, (Int, Float)) -> String
nome (n, p) = n

semArgumento :: (String, (Int, Float)) -> String
semArgumento (n, _) = n

imc :: Float -> Float -> String
imc peso altura
   | razao < magro  = "Abaixo do peso"
   | razao < normal = "Peso normal"
   | razao < gordo  = "Sobrepeso"
   | otherwise      = "Obesidade"
   where razao  = peso / altura ^ 2
         (magro, normal, gordo) = (18.5, 25.0, 30.0)
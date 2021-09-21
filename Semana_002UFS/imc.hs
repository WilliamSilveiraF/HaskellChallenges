imc :: Float -> Float -> String
imc peso altura
   | peso / (altura ^ 2) < 18.5 = "Abaixo do Peso"
   | peso / (altura ^ 2) < 25.0 = "Peso normal"
   | peso / (altura ^ 2) < 30.0 = "Sobrepeso" 
   | otherwise                  = "Obesidade"

imc2 :: Float -> Float -> String
imc2 peso altura
   | razao < magro  = "Abaixo do peso"
   | razao < normal = "Peso normal"
   | razao < gordo  = "Sobrepeso"
   | otherwise      = "Obesidade"
   where razao = peso / altura ^ 2
         magro  = 18.5
         normal = 25.0
         gordo  = 30.0
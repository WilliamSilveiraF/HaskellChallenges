fxd a
   | restode3  == 0  = "Multiplo de 3"
   | restode5  == 0  = "Multiplo de 5"
   | restode15 == 0  = "Multiplo de 3 e 5"
   | naoemult  == 0  = "Nao e multiplo por 3 e 5"
   where
         restode3  = mod a 3
         restode5  = mod a 5
         restode15 = mod a 15
         naoemult  = mod a 1
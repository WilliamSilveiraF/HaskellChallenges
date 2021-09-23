type CadastroSUS = [Cidadao]

type CPF = Integer
type Nome = String

type Cidadao = (CPF, Nome)

meubancodedados :: [(Int, String)]
meubancodedados = [(2, "carlos")]

{-comCheckagem :: Cidadao -> CadastroSUS -> CadastroSUS
comCheckagem newCitizen dataBank
       | (checaCPF == False) = error "CPF já cadastrado"
       | otherwise       = (:) newCitizen dataBank
         where
             checaCPF = check newCitizen dataBank
-}



check :: (Int, String) -> [(Int, String)] -> [(Int, String)]
check newCitizen dataBank
     | length contadorDeCPF == 0  = (:) newCitizen dataBank
     | otherwise                  = error "CPF já cadastrado"
       where
           contadorDeCPF = [c | (c, _) <- dataBank, c == fst newCitizen]
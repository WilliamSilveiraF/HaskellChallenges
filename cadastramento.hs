type CadastroSUS = [Cidadao]  --type CadastroSUS = [(Int, String), (CPF, Nome),...]

type CPF = Int
type Nome = String

type Cidadao = (CPF, Nome)

--Adicionando sem types
adicionaSUS2:: (Int, String) -> [(Int, String)] -> [(Int, String)]
adicionaSUS2 cidadaoN cadastroGeral = (:) cidadaoN cadastroGeral



--Meus Dados Pré-Existentes  CadastroSUS = [(Int, String), (CPF, Nome)...]
bancodedados :: CadastroSUS
bancodedados = [(2, "Carlos"), (5, "Teste")]


--Adicionando com Types
adicionaSUS:: Cidadao -> CadastroSUS -> CadastroSUS
adicionaSUS cidadaoNovo cadastroGeral = (:) cidadaoNovo cadastroGeral

--AdicionaSUS com checkagem
comCheckagem :: Cidadao -> CadastroSUS -> CadastroSUS
comCheckagem newCitizen generalRegister
       | (checaCPF == True) == error "CPF já cadastrado"
       | otherwise == (:) newCitizen generalRegister
         where
             checaCPF newCitizen generalRegister = 



--Exemplo Adicionando com números
addNumber :: Int -> [Int] -> [Int]
addNumber x lista = (:) x lista
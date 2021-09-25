--Construção do Meu Banco De Dados
dataBankSUS :: [Cidadao]
dataBankSUS = 
    [(2, "carlos", "Aracaju", "32152108"), 
    (3, "pablo", "Salvador", "32507880"), 
    (9, "Martins", "Muribeca", "32507881")]

type CPF = Integer
type Nome = String
type Genero = Char
type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia, Mes, Ano)
type DataNasc = Data
type Endereco = String
type Municipio = String
type Estado = String
type Telefone = String
type Email = String
type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio,
Estado, Telefone, Email)



AdicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS
AdicionaSUS newCitizen generalRegister
       | (checaCPF == True) == error "CPF já cadastrado"
       | otherwise == (:) newCitizen generalRegister
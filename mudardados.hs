type CadastroSUS = [Cidadao]

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

type Cidadao = (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)


atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS




getEndereco :: Cidadao -> Endereco
getEndereco ( _, _, _, _, citizenAddress, _, _, _, _) = citizenAddress
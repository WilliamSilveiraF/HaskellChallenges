--Meu Banco de Dados 
bancoDoCadastroSUS :: CadastroSUS
bancoDoCadastroSUS = [(2, "Carlos"), (5, "Teste")]

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

type Cidadao = (CPF, Nome)



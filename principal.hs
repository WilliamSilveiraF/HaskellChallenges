type CadastroSUS = [Cidadao]

--Construção do Meu Banco De Dados
dataBankSUS::CadastroSUS
dataBankSUS = 
    [(26716347665, "Paulo Souza", 'M', (11,10,1996),"Rua A, 202","Muribeca", "SE", "999997000", "psouza@gmail.com"),
    (87717347115, "Ana Reis",'F', (5,4,1970), "Rua B, 304","Aracaju", "SE", "999826004", "areis@gmail.com"),
    (99999999999, "Guilherme Alves", 'M', (02,07,2002),"Rua C, 405","Salgado", "SE", "999997044", "guilherme@gmail.com"),
    (88888888888, "Esmeralda Oliveira", 'F', (09,09,2003),"Rua D, 506","Lagarto", "SE", "999996025", "esmeralda@gmail.com"),
    (10101010101, "Fernanda Menezes", 'F', (01,04,2000),"Rua E, 506","Lagarto", "SE", "999996025", "esmeralda@gmail.com")]

--Tipagem
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

--         GERENCIAMENTO DE CIDADÃO         --  

--Localize meu CPF
getCPF :: Cidadao -> CPF
getCPF (cpf, _, _, _, _, _, _, _, _) = cpf


--Confira se meu CPF está no Banco de Dados
checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF seuCPF dataBankSUS =
    [item | item <- dataBankSUS, getCPF item == seuCPF] /= []


--Adicione um novo Cidadao, se ele já tiver cadastrado retorne o erro "Cidadao ja foi cadastrado."
adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS
adicionaSUS newCitizen dataBankSUS
     | (checaCPF (getCPF newCitizen) dataBankSUS == True) = error "Cidadao ja foi cadastrado."
     | otherwise                                          = (:) newCitizen dataBankSUS


--Dado o CPF e o novo ENDEREÇO, logo o dataBankSUS é atualizado.
atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS seuCPF dataBankSUS newAddress =
    [fmudarounaoCPF seuCPF pessoaData newAddress| pessoaData <- dataBankSUS]
       where
--fmudarounaoCPF sempre retornará um Cidadao no formato (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)
           fmudarounaoCPF  citizenCPF (cpfData, nomeData, genData, nascData, endData, munData, estadoData, telData, emailData) newAddress
             | citizenCPF == cpfData    =  (cpfData, nomeData, genData, nascData, newAddress, munData, estadoData, telData, emailData) 
             | otherwise                =  (cpfData, nomeData, genData, nascData, endData, munData, estadoData, telData, emailData)


--Dado o CPF e o novo TELEFONE, logo o dataBankSUS é atualizado       
atualizaTelSUS :: CPF  -> CadastroSUS -> Telefone -> CadastroSUS
atualizaTelSUS seuCPF dataBankSUS newTel =
--fmudarounaoCPF sempre retornará um Cidadao no formato (CPF, Nome, Genero, DataNasc, Endereco, Municipio, Estado, Telefone, Email)
    [fmudarounaoTEL seuCPF pessoaData newTel | pessoaData <- dataBankSUS]
      where
           fmudarounaoTEL  citizenCPF (cpfData, nomeData, genData, nascData, endData, munData, estadoData, telData, emailData) newTel
             | citizenCPF == cpfData    = (cpfData, nomeData, genData, nascData, endData, munData, estadoData, newTel, emailData)
             | otherwise                = (cpfData, nomeData, genData, nascData, endData, munData, estadoData, telData, emailData)


--Dado um CPF de uma pessoa morta por ex, então remova essa pessoa do meu dataBankSUS
removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS seuCPF dataBankSUS =
    [pessoaData | pessoaData <- dataBankSUS, fexisteounao seuCPF pessoaData]
      where
          fexisteounao citizenCPF (cpfData, nomeData, genData, nascData, endData, munData, estadoData, telData, emailData)
           | citizenCPF == cpfData        = False --se localizar não passe
           | (checaCPF seuCPF dataBankSUS == True)  = True
           | (checaCPF seuCPF dataBankSUS == False) = error "CPF não encontrado em nosso sistema, tente novamente."

--                    GERENCIAMENTO DE MUNICÍPIOS                    --
type IdadeInicial = Int
type IdadeFinal = Int
type FaixaIdade = (IdadeInicial, IdadeFinal)
type Quantidade = Int

cidadaosPorMunicipio :: CadastroSUS -> Municipio -> Quantidade
cidadaosPorMunicipio dataBankSUS municipio =
       length [pessoaData | pessoaData <- dataBankSUS, fsearchmunicipio pessoaData municipio]
       where 
           fsearchmunicipio (cpfData, nomeData, genData, nascData, endData, munData, estadoData, telData, emailData) municipioProcurado
            | (municipioProcurado == munData)    = True
            | otherwise                          = False


--cidadaosPorEstado :: CadastroSUS -> Estado -> Quantidade
--cidadaosPorMunicipioIdade :: CadastroSUS -> Municipio-> FaixaIdade -> Quantidade
--cidadaosPorEstadoIdade :: CadastroSUS -> Estado -> FaixaIdade -> Quantidade
--Construção do Meu Banco De Dados
dataBankSUS :: [Cidadao]
dataBankSUS = 
    [(2, "carlos", "Aracaju", "32152108"), 
    (3, "pablo", "Salvador", "32507880"), 
    (9, "Martins", "Muribeca", "32507881")]
--Tipagem
type CadastroSUS = [Cidadao]

type CPF = Integer
type Nome = String
type Endereco = String
type Telefone = String

type Cidadao = (CPF, Nome, Endereco, Telefone)




--Atualização de ENDEREÇO
--A pessoa dá o CPF e o novo endereço logo o dataBankSUS é atualizado.
atualizaEndSUS :: CPF -> CadastroSUS -> Endereco -> CadastroSUS
atualizaEndSUS seuCPF dataBankSUS newAddress =
--para todas as pessoas no meu banco de dados (pessoaData <- dataBankSUS),
--retorne a função "fMudarouNaoCPF" como resultado
    [fMudarouNaoCPF seuCPF pessoaData newAddress| pessoaData <- dataBankSUS]
       where
--fMUdarouNaoCPF sempre retornará um Cidadao no formato (CPF, Nome, Endereco, Telefone)
           fMudarouNaoCPF  citizenCPF (cpfData, nomeData, endData, telData) newAddress
             | citizenCPF == cpfData    =  (cpfData, nomeData, newAddress, telData) 
             | otherwise                =  (cpfData, nomeData, endData, telData)   

--Atualizacao de TELEFONE
atualizaTelSUS :: CPF  -> CadastroSUS -> Telefone -> CadastroSUS
--A pessoa dá o CPF e o novo telefone logo o dantaBankSUS é atualizado
atualizaTelSUS seuCPF dataBankSUS newTel =
--para todas as pessoas no meu banco de dados (pessoaData <-dataBankSUS),
--retorne a função "fMudarouNaoTEL" como resultado
    [fMudarouNaoTEL seuCPF pessoaData newTel | pessoaData <- dataBankSUS]
      where
--fMUdarouNaoTEL sempre retornará um Cidadao no formato (CPF, Nome, Endereco, Telefone)
           fMudarouNaoTEL  citizenCPF (cpfData, nomeData, endData, telData) newTel
             | citizenCPF == cpfData    = (cpfData, nomeData, endData, newTel)
             | otherwise                = (cpfData, nomeData, endData, telData)


--Remover do CadastroSUS
removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS seuCPF dataBankSUS =
    [fExisteouNao seuCPF pessoaData | pessoaData <- dataBankSUS]
      where
          fExisteouNao citizenCPF (cpfData, nomeData, endData, telData)
           | citizenCPF == cpfData        = ( , , ,)
           | otherwise                    = (cpfData, nomeData, endData, telData)
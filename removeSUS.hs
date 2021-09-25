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

getCPF :: Cidadao -> CPF
getCPF (cpf, _, _, _) = cpf

checaCPF :: CPF -> CadastroSUS -> Bool
checaCPF seuCPF dataBankSUS =
    [item | item <- dataBankSUS, getCPF item == seuCPF] /= []

removeSUS :: CPF -> CadastroSUS -> CadastroSUS
removeSUS seuCPF dataBankSUS =
    [pessoaData | pessoaData <- dataBankSUS, fexisteounao seuCPF pessoaData]
      where
          fexisteounao citizenCPF (cpfData, nomeData, endData, telData)
           | citizenCPF == cpfData        = False --se localizar não passe
           | (checaCPF seuCPF dataBankSUS == True)  = True
           | (checaCPF seuCPF dataBankSUS == False) = error "CPF não encontrado em nosso sistema, tente novamente."
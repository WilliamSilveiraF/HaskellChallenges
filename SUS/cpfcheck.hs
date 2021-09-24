type CadastroSUS = [Cidadao]

type CPF = Integer
type Nome = String

type Cidadao = (CPF, Nome)

meubancodedados :: [(Integer, String)]
meubancodedados = [(2, "carlos"), (3, "Kaflous"), (9, "Teste")]

adicionaSUS :: Cidadao -> CadastroSUS -> CadastroSUS
adicionaSUS newCitizen dataBank
      --Tamanho da minha lista "contadorDeCPFIguais" == 0, então adicione um novo cidadão ao meu banco de dados
      | length contadorDeCPFIguais == 0  = (:) newCitizen dataBank
      | otherwise                  = error "CPF já cadastrado"
        where
          --Para toda CIDADAO do meu dataBank,
          --Onde seu elemento na posição cpf é igual ao elemento na posição cpf do meu dataBank (fst newCitizen)
          --Adicione este cpf igual a minha lista contadorDeCPFIguais "[cpf]"
          contadorDeCPFIguais = [cpf | (cpf, _) <- dataBank, cpf == fst newCitizen]


check :: (Int, String) -> [(Int, String)] -> [(Int, String)]
check newCitizen dataBank
     | length contadorDeCPF == 0  = (:) newCitizen dataBank
     | otherwise                  = error "CPF já cadastrado"
       where
           contadorDeCPF = [c | (c, _) <- dataBank, c == fst newCitizen]
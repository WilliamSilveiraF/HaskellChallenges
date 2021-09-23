type Nome   = String
type Idade  = Int
type Altura = Float
type Pessoa = (Nome, (Idade, Altura))

nome :: Pessoa -> Nome
nome pes = fst pes

idade :: Pessoa -> Idade
idade  pes = fst (snd pes)

altura :: Pessoa -> Altura
altura pes = snd (snd pes)
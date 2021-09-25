type Nome = String
type Autor = String
type ISBN = Int

type Livro = (Nome, Autor, ISBN)
type Registro = [Livro]

registro :: Registro
registro = 
    [("Suno Dividendos","Alberto Amparo", 123), ("Suno Fiis", "Professor Baroni", 222)]

getISBN :: Livro -> ISBN
getISBN (_,_, isbn) = isbn
--   :: Int ->[(String, String, Int)]-> True or False
--   :: Int ->[(Nome, Autor, ISBN)]-> True or False
checaISBN :: ISBN -> Registro -> Bool
checaISBN isbn registro =
    [item | item <- registro, getISBN item == isbn] == []

inserirLivro :: Livro -> Registro -> Registro
inserirLivro livro registro 
    | checaISBN (getISBN livro) registro = livro:registro
    | otherwise = error "ISBN existente"

alterarAutor :: ISBN -> Registro -> Autor -> Registro
alterarAutor isbn registro novoAutor =
    [alterarLivro isbn item novoAutor | item <- registro]
      where
          alterarLivro isbnProcurado (nome, autor, isbn) novoAutor
              | (isbnProcurado == isbn)   = (nome, novoAutor, isbn)
              | otherwise                 = (nome, autor, isbn)
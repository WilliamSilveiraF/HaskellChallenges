fx :: [String] -> [String] -> [String]
fx adjetivos nomes = [nome ++ " " ++ adjetivo | adjetivo <- adjetivos, nome <- nomes]
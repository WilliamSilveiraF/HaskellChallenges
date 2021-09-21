escrevaMe :: Int -> String
escrevaMe 1 = "Um"
escrevaMe 2 = "Dois"
escrevaMe 3 = "Três"
escrevaMe 4 = "Quatro"
escrevaMe 5 = "Cinco"
escrevaMe _ = "Cansei"

testeifs :: Int -> String 
testeifs a =
    if a == 1 then "Um"
    else if a == 2 then "Dois"
    else if a == 3 then "Tres"
    else "Cansei"
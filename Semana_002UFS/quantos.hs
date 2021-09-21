--quantos são iguais
quantos a b c
    | a == b && b == c  = 3
    | a == b || b == c  = 2
    | otherwise         = 0
-- quantos são unicos 
unicos a b c
    | (a == b) && (b == c)              = 0
    | (a == b) || (a == c) || (b == c)  = 1
    | otherwise                         = 3

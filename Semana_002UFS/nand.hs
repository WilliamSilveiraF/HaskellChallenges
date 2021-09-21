nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd True False = True
nAnd False True = True
nAnd False False = True

nImporta :: Bool -> Bool -> Bool
nImporta True True = False
nImporta _ _ = True
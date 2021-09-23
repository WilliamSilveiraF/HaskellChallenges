import Data.Char

teste :: String -> String
teste listaDeCh = [snum | snum <- listaDeCh, not (ehDigito snum)]
     where
         ehDigito x =  (x <= '9') && (x >= '0')

--ehDigito :: Char -> Bool
--ehDigito x =  (x <= '9') && (x >= '0')
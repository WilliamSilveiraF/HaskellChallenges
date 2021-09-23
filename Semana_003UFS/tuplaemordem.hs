ordem :: Int -> Int -> Int -> (Int, Int, Int)
ordem x y z
     | x == y && x == z  =  (x, x, x)
     | otherwise         =  (vmenor, vmeio, vmaior)
       where
           vmaior = max x (max y z)
           vmeio 
            | vmenor < x && x < vmaior = x
            | vmenor < y && y < vmaior = y
            | vmenor < z && z < vmaior = z
            | otherwise                = 0
           vmenor = min x (min y z)

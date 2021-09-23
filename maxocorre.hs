maxOcorre :: Int -> Int -> (Int, Int)
maxOcorre x y
    | x == y    = (x, 2)
    | otherwise = ((max x y), 1)


ocorre3vezes :: Int -> Int -> Int -> (Int, Int)
ocorre3vezes x y z
   | x == y && x == z  = (x, 3)

   | x == y &&  x > z  = (x, 2)
   | x == y &&  z > x  = (z, 1)

   | x == z &&  x > y  = (x, 2)
   | x == z &&  y > x  = (y, 1)

   | z == y &&  x > z  =  (x, 1)
   | z == y &&  z > x  =  (z, 2)
   | otherwise         =  ((max x(max z y)), 1)
ordem :: Int -> Int -> Int -> (Int, Int, Int)
ordem x y z
     | x == y && x == z  = (x, y, z)

     | x == y &&  x > z  = (z, x, x)
     | x == y &&  z > x  = (x, x, z)

     | x == z &&  x > y  = (y, x, x)
     | x == z &&  y > x  = (x, x, y)

     | z == y &&  x > z  = (z, z, x)
     | z == y &&  z > x  = (x, z, z)


     | x > y  &&  y > z  =  (z, y, x)
     | z > y  &&  y > x  =  (x, y, z)

     | y > x  &&  x > z  =  (z, x, y)
     | z > x  &&  x > y  =  (y, x, z)

     | x > z  &&  z > y  =  (y, z, x)
     | y > z  &&  z > x  =  (x, z, y)

     | max(x, max y z)
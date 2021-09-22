import CodeWorld

main = animationOf bolaHV30

bolaHV30 :: Double -> Picture
bolaHV30 t = bolaHorizontal t & bolaVertical t & bolaEm30 t

bolaEm30 :: Double -> Picture
bolaEm30 t = translated (-10 + 4 * cos (pi/6) * t)
                        (-10 + 2 * t) bola
             
{- bolaEm30 t = translated x y bola
    where
    (x, y) = vectorSum (-10, -10) (scaledVector (m *t) (cos (pi/6), 0.5)
-}
             
             
             
             
bolaHorizontal :: Double -> Picture
bolaHorizontal t = translated (-10 +2*t) 0 bola

bolaVertical :: Double -> Picture
bolaVertical t = translated 0 (10 - 1/2 * g * t^2) bola                        
         where g = 3
         
bola = colored red (solidCircle 1)
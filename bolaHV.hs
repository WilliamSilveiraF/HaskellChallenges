import CodeWorld

main = animationOf bolaHV

bolaHV :: Double -> Picture
bolaHV t = bolaHorizontal t & bolaVertical t

bolaHorizontal :: Double -> Picture
bolaHorizontal t = translated (-10 + 2*t) 0 bola

bolaVertical :: Double -> Picture
bolaVertical t = translated 0 (10 - 1/2 * g * t ^2) bola
       where g = 1

bola = colored red (solidCircle 1)
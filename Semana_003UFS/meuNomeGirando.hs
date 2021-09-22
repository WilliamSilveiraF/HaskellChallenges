{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main = animationOf meuNomeGirando 

meuNome = dilated 2 (lettering "WILLIAM")

meuNomeGirando :: Double -> Picture
meuNomeGirando t = rotated (pi/9 * t) meuNome
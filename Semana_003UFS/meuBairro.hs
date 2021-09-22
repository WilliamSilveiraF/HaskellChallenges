import CodeWorld

main = drawingOf bairro

casa :: Color -> Picture
casa cor = teto & porta & parede
     where parede = colored cor (solidRectangle 4 3)
           teto   = colored orange (solidPolygon [(0, 3), (-3, 1), (3, 1)])
           porta  = blank
     
rua :: Color -> Color -> Color -> Picture
rua c1 c2 c3 = translated (-5) 0 (casa c1) &
               casa c2 &
               translated 5 0 (casa c3)
               
bairro = rua yellow pink blue &
         dilated 0.7 (translated 0 4 (rua red blue green))
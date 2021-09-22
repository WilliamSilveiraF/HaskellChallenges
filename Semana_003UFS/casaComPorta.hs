import CodeWorld

main = drawingOf (coordinatePlane & casa)

casa = teto & porta & parede
   where parede = colored red (solidRectangle 4 3)
         teto = colored orange (solidPolygon [(0, 3), (-3, 1), (3, 1)])
         porta =  translated 1 (-0.5) (solidRectangle 0.75 2)


minhaFigura = translated 0 3 (rotated (pi/4) (solidRectangle 4 2))
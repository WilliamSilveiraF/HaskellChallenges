verdadeiro :: Bool -> Bool -> Bool
verdadeiro a b = a || b

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myExOr :: Bool -> Bool -> Bool
myExOr True x = not x
myExOr False x = x
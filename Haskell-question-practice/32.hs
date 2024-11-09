myGCD x 0 = x
myGCD x 1 = 1
myGCD x y 
    | y > x = myGCD y x
    | x `mod` y == 0 = y
    | otherwise = myGCD y (x `mod` y)
primeFactorIter :: Int -> Int -> [Int]
primeFactorIter n i
    | i > n = []
    | n `mod` i == 0 = [i] ++ (primeFactorIter (n `div` i) i)
    | otherwise = primeFactorIter n (i+1)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = primeFactorIter n 2
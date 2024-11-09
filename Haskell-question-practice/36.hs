primeFactorIter :: Int -> Int -> [Int]
primeFactorIter n i
    | i > n = []
    | n `mod` i == 0 = [i] ++ (primeFactorIter (n `div` i) i)
    | otherwise = primeFactorIter n (i+1)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = primeFactorIter n 2

run_length_encode :: (Eq a) => [a] -> [(a, Int)]
run_length_encode [] = []
run_length_encode [x] = [(x, 1)]
run_length_encode (h:hs)
    | h == (head hs) = [(h, (snd (head (run_length_encode hs)))+1)] ++ (tail (run_length_encode hs))
    | otherwise = [(h, 1)] ++ run_length_encode hs

primeFactorMult :: Int -> [(Int, Int)]
primeFactorMult n = run_length_encode (primeFactors n)

sum_factors n i   
    | i >= n = 0
    | mod n i == 0 = i + (sum_factors n (i + 1))
    | otherwise =  sum_factors n (i + 1)

whether_perfect n = sum_factors n 1 == n

perfect_iter i n
    | i > n = []
    | whether_perfect i = [i] ++ perfect_iter (i+1) n
    | otherwise = perfect_iter (i+1) n

perfects n = perfect_iter 1 n
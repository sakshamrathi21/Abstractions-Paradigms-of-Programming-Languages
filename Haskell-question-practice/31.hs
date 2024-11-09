is_prime_iter n i
    | i*i > n = True
    | n `mod` i == 0 = False
    | otherwise = is_prime_iter n (i + 1)

is_prime 1 = False
is_prime n = is_prime_iter n 2

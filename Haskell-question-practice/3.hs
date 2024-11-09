element_at :: [a] -> Int -> a
element_at [x] p = x
element_at (x:xs) p
    | p == 1 = x
    | otherwise = element_at xs (p-1)
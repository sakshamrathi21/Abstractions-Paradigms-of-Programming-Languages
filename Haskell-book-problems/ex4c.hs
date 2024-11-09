disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs [] = False
disjoint [] xs = False
disjoint (x:xs) (y:ys)
    | x == y = True
    | x < y = disjoint xs (y:ys)
    | otherwise = disjoint (x:xs) ys

encode :: (Eq a) => [a] -> [(Int, a)]
encode [x] = [(1, x)]
encode (x:xs)
    | (x == (head xs)) = [(fst (head (encode xs)) + 1, x)] ++ tail (encode xs)
    | otherwise = [(1, x)] ++ (encode xs)
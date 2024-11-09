slice [] a b = []
slice (h:hs) 0 b = slice (h:hs) 1 b
slice (h:hs) a b
    | a > b = []
    | a == 1 = [h] ++ (slice hs 1 (b-1))
    | otherwise = slice hs (a - 1) (b - 1)
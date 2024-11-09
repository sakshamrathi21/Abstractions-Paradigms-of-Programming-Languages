repli_iter [] x y = []
repli_iter (h:hs) x y 
    | y == 0 = repli_iter hs x x
    | otherwise = [h] ++ repli_iter (h:hs) x (y - 1)
repli l x = repli_iter l x x
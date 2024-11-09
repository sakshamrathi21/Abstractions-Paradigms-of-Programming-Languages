-- Reverse a list
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]
insert :: Ord a => a -> [a] -> [a] 
insert x [] = [x]
insert x l = if (head l) > x then ([x] ++ l) else [(head l)] ++ (insert x (tail l))

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
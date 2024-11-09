import Data.Char
main :: IO ()    -- This says that main is an IO action.
main = putStrLn $ commonWords 5 "hello world World"

type Text = [Char]
type Word = [Char]

showRun :: (Int, Main.Word) -> String
showRun (n,w) = w ++ ": " ++ show n ++ "\n"

span :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])
span p (x:xs) = if p x then (x:ys,zs)
                else ([],x:xs)
                where (ys,zs) = Main.span p xs

countRuns :: [Main.Word] -> [(Int,Main.Word)]
countRuns [] = []
countRuns (w:ws) = (1+Prelude.length us,w) : countRuns vs
                                    where (us,vs) = Main.span (==w) ws

sort [] = []
sort [x] = [x]
sort xs = merge (sort ys) (sort zs)
 where (ys,zs) = half xs
half xs = (Prelude.take n xs, Prelude.drop n xs)
 where n = Prelude.length xs `div` 2
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 | x <= y = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

sortWords :: [Main.Word] -> [Main.Word]
sortWords = sort
sortRuns :: [(Int,Main.Word)] -> [(Int,Main.Word)]
sortRuns = Prelude.reverse . sort

commonWords :: Int -> Main.Text -> String
commonWords n = Prelude.concat . Prelude.map showRun . Prelude.take n . sortRuns . countRuns . sortWords . Prelude.words . Prelude.map toLower

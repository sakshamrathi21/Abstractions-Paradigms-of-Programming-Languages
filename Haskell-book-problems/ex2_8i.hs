import Data.Char (isAlpha, toLower)

palindrome :: String -> String -> Bool
palindrome [] [] = True
palindrome (x:xs) (y:ys)
    | not (isAlpha x) = palindrome xs (y:ys)
    | not (isAlpha y) = palindrome (x:xs) ys
    | toLower x == toLower y = palindrome xs ys
    | otherwise = False
palindrome _ _ = False

main :: IO ()
main = do
    putStrLn "Enter a string: "
    st <- getLine
    let sr = reverse st
    if palindrome st sr then putStrLn "Yes!"
    else putStrLn "No!"

import Data.Char
type Word = String
modernise :: String -> String
modernise = unwords . map capitalise . words
capitalise :: Main.Word -> Main.Word
capitalise xs = [toUpper (head xs)] ++ tail xs
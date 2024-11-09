stringsum st | st == "" = 0
             | otherwise = read (take 1 st) + stringsum (drop 1 st)
addSum :: String -> String
addSum st = st ++ show (stringsum st)

validCIN :: String -> Bool
validCIN st = if (addSum (take 8 st)) == st then True
              else False

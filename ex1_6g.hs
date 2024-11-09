song n = if n == 0 then ""
         else song (n-1) ++ "\n" ++ verse n
verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

line1 n = (word!! (n-1)) ++ " man went to mow\n"
line2 n = "Went to mow a meadow\n"
line3 n = (word!! (n-1)) ++ " man and his dog\n"
line4 n = "Went to mow a meadow\n"

word = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]

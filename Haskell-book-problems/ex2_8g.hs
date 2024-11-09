suff date = if (date `mod` 10 == 1) then "st"
            else if (date `mod` 10 == 2) then "nd"
            else if (date `mod` 10 == 3) then "rd"
            else "th"
month_names = ["January","February","March","April","May","June","July","August","September","October","November","December"]
showDate (date,month,year) = show date ++ suff date ++ " " ++ month_names!!(month - 1) ++ ", " ++ show year

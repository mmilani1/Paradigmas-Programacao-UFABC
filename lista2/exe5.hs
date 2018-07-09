palindromo :: [Int] -> Bool
palindromo []    = True
palindromo [a]   = True
palindromo as
    | head as == last as    = palindromo $ init $ tail as
    | otherwise             = False
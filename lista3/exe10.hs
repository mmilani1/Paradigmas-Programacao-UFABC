skips :: [a] -> [[a]]
skips [] = []
skips (a:[]) = [[a]]
skips as = skips' 1 as
    where
        skips' c as
            | c <= length as    = [x | (x,ind) <- zip as [1..], ind `mod` c == 0] : skips' (c+1) as
            | otherwise         = []
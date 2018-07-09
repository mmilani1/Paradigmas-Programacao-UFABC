coord :: [a] -> [a] -> [(a,a)]
coord x y = [(i,j) | i <- x | j <- y]

coord' :: [a] -> [a] -> [(a,a)]
coord' x y = concat [[(i,j) | j <- y] | i <- x]
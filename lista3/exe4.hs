concat' :: [[a]] -> [a]
concat' [[]]  = []
concat' [[a]] = [a]
concat' ass = concat'' [] ass
    where
        concat'' resp [[]]     = resp
        concat'' resp [[a]]    = resp++[a]
        concat'' resp (as:ass) = concat'' (resp++as) ass

concat2 :: [[a]] -> [a]
concat2 [[]]         = []
concat2 ([]:ass)     = concat2 ass
concat2 ((a:as):ass) = a:concat2 (as:ass)
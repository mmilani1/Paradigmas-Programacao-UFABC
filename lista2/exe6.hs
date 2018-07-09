ordenaListas :: (Num a, Ord a) => [[a]] -> [[a]]
ordenaListas [] = []
ordenaListas (x:xs) = ordenaListas menores ++ [x] ++ ordenaListas maiores
    where
        menores = [a | a <- xs, length a <= length x]
        maiores = [b | b <- xs, length b > length x]
--------------------------------------------------------
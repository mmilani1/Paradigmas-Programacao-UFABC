perfeitos :: Int -> [Int]
perfeitos n = [p | p <- [1..n], perfeito p]

perfeito :: Int -> Bool
perfeito n
    | sum (fatoresP n) == n = True
    | otherwise         = False

fatoresP :: Int -> [Int]
fatoresP n = [x | x <- [1..(n-1)], fator n x]

fator :: Int -> Int -> Bool
fator n x
    | n `mod` x == 0    = True
    | otherwise         = False
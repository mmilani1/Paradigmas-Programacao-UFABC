digitosRev :: Int -> [Int]
digitosRev n 
    | n > 0     =  (n `mod` 10):digitosRev (n`div`10)
    |otherwise  = []

dobroAlternado :: [Int] -> [Int]
dobroAlternado [] = []
dobroAlternado [a] = [a]
dobroAlternado (a:b:ns) = (a:(2*b):(dobroAlternado ns))

somaDigitos :: [Int] -> Int
somaDigitos ns = sum [sum $ digitosRev x | x <- ns]

luhn :: Int -> Bool
luhn n
    | total `mod` 10 == 0 = True
    | otherwise           = False
    
    where
        total   = somaDigitos dobro
        dobro   = dobroAlternado digitos
        digitos = digitosRev n
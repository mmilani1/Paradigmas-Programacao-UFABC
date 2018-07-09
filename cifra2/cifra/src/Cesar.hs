module Cesar where

import Data.Char

--converte uma letra minuscula para inteiro
let2int :: Char -> Int
let2int c = ord c - ord 'a'

--converte um inteiro para letra minuscula
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

--retorna a n-esima letra seguinte,
-- evite ultrapassar o limite com `mod` 26
shift :: Int -> Char -> Char
shift n c | isLower c = int2let (((let2int c) + n) `mod` 26)
          | otherwise = c
          

-- aplica a funcao shift para cada letra dda string
encode :: Int -> String -> String
encode 0 cs = cs
encode n cs = [shift n c | c <- cs]

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor  = head (positions (minimum chitab) chitab)
        chitab  = [chisqr (rotate n table') table | n <- [0..25]]
        table'  = freqs xs

-- quantidade de letras minusculas em uma string
lowers :: String -> Int
lowers [] = 0
lowers (c:cs) | isLower c   = 1 + lowers cs
              | otherwise   = lowers cs

-- conta a ocorrencia de um caracter em uma String
count :: Char -> String -> Int
count _ [] = 0
count x (c:cs) | x == c         = 1 + count x cs
               | otherwise      = count x cs

-- dado um n e m, calcule 100 * (n/m)
percent :: Int -> Int -> Float
percent n m = 100 * (fromIntegral n / fromIntegral m)

-- calcula a porcentagem de cada letra minuscula
-- do alfabeto em uma string
-- a porcentagem eh a contagem de ocorrencia pelo total de
-- letras minusculas
freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a'..'z']]

-- calcula a media de ChiSquared de duas
-- tabelas de frquencia:
-- Soma (Observado - Esperado)^2 / Esperado
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum final
    where
        final = [((o-e)^2)/e | (o,e) <- zip os es]

-- rotaciona uma tabela em n posicoes
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 a = a
rotate n as = drop n as ++ take n as

-- retorna uma lista de posicoes que contem
-- elemento x
positions :: Eq a => a -> [a] -> [Int]
positions _ [] = []
positions x as = [fst p | p <- zip [0..] as, x == snd p]

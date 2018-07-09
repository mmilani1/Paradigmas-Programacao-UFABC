module Cesar where

import Data.Char -- converte uma letra minuscula para inteiro
import Data.List

let2int :: Char -> Int
let2int c = ord c - ord 'a' -- converte um inteiro para letra minuscula

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

table :: [Float]
table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,
         0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,
         6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

-- retrona a n-esima letra seguinte
-- evire ultrapassar o limite com `mod` 26
shift :: Int -> Char -> Char
shift _ ' ' = ' '
shift n a
  | n >= 0   = int2let((n+b) `mod` 26) 
  | n < 0   = int2let((n+b) `mod` 26)
  where
    b = let2int(a)


-- aplica a funcao shift em cada letra da string
encode :: Int -> String -> String
encode n xs = [(shift n x) | x <- xs]


crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table
                      | n <- [0..25]]
    table' = freqs xs

--quantidade de letras minusculas em uma String
lowers :: String -> Int
lowers [] = 0
lowers (a:bs)
  | isLower(a)  = 1 + lowers bs
  | otherwise   = 0 + lowers bs

--conta a ocorrencia de um caracter em uma string
count :: Char -> String -> Int
count _ [] = 0
count c (b:bs)
  | c == b    = 1 + count c bs
  | otherwise = 0 + count c bs

--dado um n e m, calcula 100(n/m)
percent :: Int->Int->Float
percent n m = 100*(fromIntegral n) / (fromIntegral m)

--calcula a frenquencia de cada letra minuscula
--do alfabeto em uma String
--a porcentagem e a contagem de ocorrencia pelo total 
--de letras minusculas
freqs :: String -> [Float]
freqs [] = []
freqs cs = [(fromIntegral (count c cs)) / (fromIntegral (lowers cs)) | c <- cs]


-- Calcule a medida de Chi-Squared de duas
-- tabelas de frequência:
-- Soma (Observado - Esperado)^2 / Esperado
chisqr :: [Float]->[Float]->Float
chisqr to te = sum chitable
  where
    chitable = [((fo - fe)^2)/fe | fo <- to, fe <- te]

--rotaciona uma tabela em n posicoes
rotate ::Int->[a]->[a]
rotate n fs = (drop n fs) ++ (take n fs)

--retorna lista de posicoes contendo um char a
positions :: Eq a => a -> [a] -> [Int]
positions a as = map (+1) (elemIndices a as)
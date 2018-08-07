module Main where

import Data.Function (on)
import Data.List (sortBy)
import System.IO

first :: (a,b,c) -> a
first(x,_,_) = x

second :: (a,b,c) -> b
second(_,x,_) = x

third :: (a,b,c) -> c
third(_,_,x) = x

mySort :: Ord b => [(a, b, c)] -> [(a, b, c)]
mySort = sortBy (flip compare `on` second)

removePunc :: String -> String
removePunc xs = filter (/='\n') xs

--[ x | x <- xs, not (x `elem` " '\n',.?!-_+:;\'/'<>") ]

--finaltupla :: [Char] -> [String] -> [(Char,String)]
--finaltupla tc td ts =  zip3  tc td ts

--comp ::  Int
--comp   = length text

--tuplachar :: (Char,Double,String) -> Char
--tuplachar tc = first tc

--tupladouble :: (Char,Double,String) -> Double
--tupladouble td = second td

prob :: String -> Char -> Int -> (Char,Double,String)
prob s d comp
        | existe > 0  = (d, fromIntegral existe/fromIntegral comp,"")
        | otherwise = (d, 0.0,"")
    where
        existe = length (filter (==d) s)

probabilidades :: String -> Int -> [(Char,Double,String)]
probabilidades [] _ = []
probabilidades s  comp = mySort (prob s (head s) comp : probabilidades (filter (/=head s) s) comp)

data Tree = Leaf String | Node [String] (Tree) (Tree)
              deriving (Eq, Ord, Show, Read)

--prop = [p | (c,p,s) <- probabilidades text comp]
--cod  = [s | (c,p,s) <- probabilidades text comp]

--arvore :: [Double] -> Tree a
--arvore [a] = Leaf [a]
--arvore prop = Node prop (arvore (take i prop)) (arvore (drop i prop))
--    where
--       i = quebra prop

arvore :: [String] -> [Double] -> Tree
arvore [a] _ = Leaf a
arvore cod prop = Node cod (arvore (map (++"0") (take i cod)) (take i prop)) (arvore (map (++"1") (drop i cod)) (drop i prop))
    where
        i = quebra prop

listaFolhas :: Tree -> [String]
listaFolhas (Leaf a) = [a]
listaFolhas (Node n esq dir) = listaFolhas esq ++ listaFolhas dir
 
quebra :: [Double] -> Int
quebra (p1:ps) = quebra' (p1:ps) 1 (abs(p1-(sum ps)))
    where
        quebra' [] i diff = 0
        quebra' (p1:p2:ps) i diff  | abs((p1+p2)-sum ps) >= diff = i
                                   | otherwise                   = quebra' ((p1+p2):ps) (i+1) (abs((p1+p2)-sum ps))

subst :: String -> [(Char,String)] -> String  
subst [] cod     = []
subst (l:ls) cod = head ( [c | (a,c) <- cod, l == a] ) ++ subst ls cod

encode :: FilePath -> IO (String,String)
encode fileName = do
    inputHandle         <- openFile fileName ReadMode
    text                <- hGetContents inputHandle
    let newtext         = removePunc text
    let comp            = length newtext
    let listaProb       = [p | (c,p,s) <- probabilidades (text) comp]
    let listaCodVazio   = [s | (c,p,s) <- probabilidades (text) comp]
    let listaChar       = [c | (c,p,s) <- probabilidades newtext comp]
    let listaCod        = listaFolhas $ arvore listaCodVazio listaProb
    let listaTuplaFinal = zip listaChar listaCod
    return (text, subst newtext listaTuplaFinal)

main :: IO ()
main = do
    (original, codificado) <- encode "teste.txt"
    handleOutput <- openBinaryFile "codificado.b" WriteMode
    hPutStr handleOutput codificado
    print (original ++ " -> " ++ codificado)

module Main where

import Data.Function (on)
import Data.List     (sortBy)
import Data.List     (nub)
import System.IO
import System.Environment

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

prob :: String -> Char -> Int -> (Char,Double,String)
prob s d comp
        | existe > 0  = (d, fromIntegral existe/fromIntegral comp,"")
        | otherwise = (d, 0.0,"")
    where
        existe = length (filter (==d) s)

--letras :: String -> [Char]
--letras conteudo = nub conteudo

probabilidades :: String -> Int -> [(Char,Double,String)]
probabilidades [] _    = []
probabilidades s  comp = mySort (prob s (head s) comp : probabilidades (filter (/=head s) s) comp)

data Tree = Leaf String | Node [String] (Tree) (Tree)
              deriving (Eq, Ord, Show, Read)

type Codificacao = [(Char, String)]

--prop = [p | (c,p,s) <- probabilidades text comp]
--cod  = [s | (c,p,s) <- probabilidades text comp]

--arvore :: [Double] -> Tree a
--arvore [a] = Leaf [a]
--arvore prop = Node prop (arvore (take i prop)) (arvore (drop i prop))
--    where
--       i = quebra prop

arvore :: [String] -> [Double] -> Tree
arvore [a] _    = Leaf a
arvore cod prop = Node cod (arvore (map (++"0") (take i cod)) (take i prop)) (arvore (map (++"1") (drop i cod)) (drop i prop))
    where
        i = quebra prop

listaFolhas :: Tree -> [String]
listaFolhas (Leaf a)         = [a]
listaFolhas (Node n esq dir) = listaFolhas esq ++ listaFolhas dir

quebra :: [Double] -> Int
quebra (p1:ps) = quebra' (p1:ps) 1 (abs(p1-(sum ps)))
    where
        quebra' [] i diff = 0
        quebra' (p1:p2:ps) i diff  | abs((p1+p2)-sum ps) >= diff = i
                                   | otherwise                   = quebra' ((p1+p2):ps) (i+1) (abs((p1+p2)-sum ps))

eSubst :: String -> Codificacao -> String
eSubst [] cod     = []
eSubst (l:ls) cod = head ( [c | (a,c) <- cod, l == a] ) ++ eSubst ls cod

--dSubst :: String -> [(Char,String)] -> String
--dSubst [] cod     = []
--dSubst (l:ls) cod | l == head ( [a | (a,c) <- cod, l == head c] )  =  head ( [c | (a,c) <- cod, l == a] ) ++ dSubst ls cod
--                  | otherwise = dSubst ls cod

dic :: String -> Codificacao -> String
dic "" _                = ""
dic codigo listaCharCod = [l | (l,c) <- listaCharCod, codigo == c]

dSubst :: String -> Codificacao -> String
dSubst [] _     = []
dSubst s cod    = dSubst' s cod "" ""
    where
        dSubst' [] cod teste decod       = decod ++ (dic teste cod)
        dSubst' (s:ss) cod teste decod   | (dic teste cod) /= "" = dSubst' (s:ss) (cod) ("") (decod ++ (dic teste cod))
                                         | otherwise             = dSubst' ss cod (teste ++ [s]) decod

gerador :: String -> IO (String, Codificacao)
gerador fileName = do
    inputHandle                 <- openFile fileName ReadMode
    text                        <- hGetContents inputHandle
    let newtext                 =  text
    let comp                    =  length newtext
    let listaProb               =  [p | (c,p,s) <- probabilidades newtext comp]
    let listaCodVazio           =  [s | (c,p,s) <- probabilidades newtext comp]
    let listaChar               =  [c | (c,p,s) <- probabilidades newtext comp]
    let listaCod                =  listaFolhas $ arvore listaCodVazio listaProb
    let listaCharCod            =  zip listaChar listaCod
    handleOutput                <- openBinaryFile "codificacao.txt" WriteMode
    hPutStr handleOutput (show listaCharCod)
    hClose handleOutput
    hClose inputHandle
    return (newtext, listaCharCod)

encode :: String -> IO()
encode inputFileName = do
    (t_original,listaCharCod)           <- gerador inputFileName
    let t_codificado                    =  eSubst t_original listaCharCod
    handleOutput                        <- openBinaryFile "codificado.txt" WriteMode
    hPutStr handleOutput t_codificado
    hClose handleOutput

decode :: String -> String -> IO ()
decode encodedFileName hashFileName = do
    t_codificado                            <- readFile encodedFileName
    handlelistaCharCod                      <- readFile hashFileName
    let listaCharCod                        =  read handlelistaCharCod :: Codificacao
    let t_decodificado                      =  dSubst t_codificado listaCharCod
    handleOutput                            <- openBinaryFile "decodificado.txt" WriteMode
    hPutStr handleOutput t_decodificado
    hClose handleOutput

main = do
  args <- getArgs
  if args == [] then do

    putStrLn ("Instruções: ")
    putStrLn ("Para codificar, execute stack exec -- shannon e (nome do arquivo de texto)")
    putStrLn ("Para decodificar, execute stack exec -- shannon d (nome do arquivo codificado) (nome do arquivo com a lista de codigos)")

    else
      if (args !! 0 == "encode") then do

        let nomeTexto = args !! 1
        encode nomeTexto

        else
          if (args !! 0 == "decode") then do

            let codificado = args !! 1
            let codificacao = args !! 2
            decode codificado codificacao

            else
              putStrLn "Argumentos errados"

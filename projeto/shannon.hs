module Main where

import Data.Function (on)
import Data.List     (sortBy)
import Data.List     (nub)
import System.IO
import System.Environment


-- Retorna o primeiro valor de uma tupla
first :: (a,b,c) -> a
first(x,_,_) = x


-- Retorna o segundo valor de uma tupla
second :: (a,b,c) -> b
second(_,x,_) = x


-- Retorna o terceiro valor de uma tupla
third :: (a,b,c) -> c
third(_,_,x) = x


-- Função capaz de ordenar listas de tuplas com base e algum dos valores revelantes
-- das tuplas.
mySort :: Ord b => [(a, b, c)] -> [(a, b, c)]
mySort = sortBy (flip compare `on` second)


-- A função 'prob' faz o calculo da probabilidade de um caracter em uma string
-- e armazena em um tupla de três valores, um caracter, o valor de probabilidade
-- e uma string vazia, que será util na geração dos codigos Shannon-Fano
prob :: String -> Char -> Int -> (Char,Double,String)
prob s d comp
        | existe > 0  = (d, fromIntegral existe/fromIntegral comp,"")
        | otherwise   = (d, 0.0,"")
    where
        existe = length (filter (==d) s)


-- Função que extende a função 'prob' para uma string inteira. Ela armazenara uma 
-- lista de tuplas, cada uma com um caracter, um valor de probabilidade e uma string vazia.
-- Esta lista de tuplas é então ordenada com base nas probabilidades para que possamos
-- aplicar o algoritmo de Shannon-Fano 
probabilidades :: String -> Int -> [(Char,Double,String)]
probabilidades [] _    = []
probabilidades s  comp = mySort (prob s (head s) comp : probabilidades (filter (/=head s) s) comp)


-- Definição da árvore
data Tree = Leaf String | Node [String] (Tree) (Tree)
              deriving (Eq, Ord, Show, Read)


-- Definiçào de um novo tipo, que srmazena um caracter e seu respectivo codigo
-- Shannon-Fano
type Codificacao = [(Char, String)]


--Função de geração da arvore de codificação. Recebe uma lista de Strings vazias,
-- que armazenarão os codigos, e a lista ordenada de probabilidade de cada caracter.
-- Ao final da execução, os codigos para cada letra estarão nas folhas, respeitando a ordenação
arvore :: [String] -> [Double] -> Tree
arvore [a] _    = Leaf a
arvore cod prop = Node cod (arvore (map (++"0") (take i cod)) (take i prop)) (arvore (map (++"1") (drop i cod)) (drop i prop))
    where
        i = quebra prop


-- Função que retona uma lista com os conteudos de todas as folhas da arvore.
listaFolhas :: Tree -> [String]
listaFolhas (Leaf a)         = [a]
listaFolhas (Node n esq dir) = listaFolhas esq ++ listaFolhas dir


-- Principal função que determina o ponto de separação dos ramos da arvore com base
-- nas probabilidades e nas regras do algoritmo de Shannon-Fano
quebra :: [Double] -> Int
quebra (p1:ps) = quebra' (p1:ps) 1 (abs(p1-(sum ps)))
    where
        quebra' [] i diff = 0
        quebra' (p1:p2:ps) i diff  | abs((p1+p2)-sum ps) >= diff = i
                                   | otherwise                   = quebra' ((p1+p2):ps) (i+1) (abs((p1+p2)-sum ps))


-- Função responsavel por executar a substituição dos caracteres lidos por seus
-- respectivos codigos no processo de codificação
eSubst :: String -> Codificacao -> String
eSubst [] cod     = []
eSubst (l:ls) cod = head ( [c | (a,c) <- cod, l == a] ) ++ eSubst ls cod


-- Função criada para simular o recurso de dicionarios, presente em Python.
-- utilizada para resgatar o segundo valor armazenado em uma tupla, sabendo o
-- o primeiro valor.
dic :: String -> Codificacao -> String
dic "" _                = ""
dic codigo listaCharCod = [l | (l,c) <- listaCharCod, codigo == c]

-- Função responsavel por executar a substituição dos codigos lidos por suas
-- respectivas letras no processo de decodificação
dSubst :: String -> Codificacao -> String
dSubst [] _     = []
dSubst s cod    = dSubst' s cod "" ""
    where
        dSubst' [] cod teste decod       = decod ++ (dic teste cod)
        dSubst' (s:ss) cod teste decod   | (dic teste cod) /= "" = dSubst' (s:ss) (cod) ("") (decod ++ (dic teste cod))
                                         | otherwise             = dSubst' ss cod (teste ++ [s]) decod

-- Esta função é executada toda vez que a função encode é chamdada. Ela fará
-- a leitura do arquivo de texto original, avaliará as informações necessarias
-- para o nosso algoritmo (comprimento, lista exclusiva de caracteres ordenada,
-- lista de probabilidades ordenada, criará uma lista de strings vazia para a
-- geração dos codigos). Ela também salva em um arquivo externo uma lista de tuplas
-- onde estará todos os pares de caracter com seus respectivos codigos, para que possam 
-- ser reutilizados em uma eventual decodificação.
gerador :: String -> IO (String, Codificacao)
gerador fileName = do
    inputHandle                 <- openFile fileName ReadMode
    text                        <- hGetContents inputHandle
    let comp                    =  length text
    let listaProb               =  [p | (c,p,s) <- probabilidades text comp]
    let listaCodVazio           =  [s | (c,p,s) <- probabilidades text comp]
    let listaChar               =  [c | (c,p,s) <- probabilidades text comp]
    let listaCod                =  listaFolhas $ arvore listaCodVazio listaProb
    let listaCharCod            =  zip listaChar listaCod
    handleOutput                <- openBinaryFile "codificacao.txt" WriteMode
    hPutStr handleOutput (show listaCharCod)
    hClose handleOutput
    hClose inputHandle
    return (text, listaCharCod)

-- Função de codificação de um arquivo de texto.
encode :: String -> IO()
encode inputFileName = do
    (t_original,listaCharCod)           <- gerador inputFileName
    let t_codificado                    =  eSubst t_original listaCharCod
    handleOutput                        <- openBinaryFile "codificado.txt" WriteMode
    hPutStr handleOutput t_codificado
    hClose handleOutput

-- Função de decodificação de um arquivo já codificado. Necessario ter o arquivo
-- 'codificacao.txt', gerado na função gerador.
decode :: String -> String -> IO ()
decode encodedFileName hashFileName = do
    t_codificado                            <- readFile encodedFileName
    handlelistaCharCod                      <- readFile hashFileName
    let listaCharCod                        =  read handlelistaCharCod :: Codificacao
    let t_decodificado                      =  dSubst t_codificado listaCharCod
    handleOutput                            <- openBinaryFile "decodificado.txt" WriteMode
    hPutStr handleOutput t_decodificado
    hClose handleOutput

-- Função main que, quando executado pelo stack, necessitara de alguns argumentos para que
-- sejam executadas as funções de codificação e decodificação
main = do
  args <- getArgs
  if args == [] then do

    putStrLn ("Instruções: ")
    putStrLn ("Para codificar, execute stack exec -- shannon encode (nome do arquivo de texto)")
    putStrLn ("Para decodificar, execute stack exec -- shannon decode (nome do arquivo codificado) (nome do arquivo com a lista de codigos)")

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

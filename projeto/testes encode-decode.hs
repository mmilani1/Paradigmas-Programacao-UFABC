import Data.List
import System.IO

subst :: String -> [(Char,String)] -> String  
subst [] cod = []
subst (l:ls) cod = head ( [c | (a,c) <- cod, l == a] ) ++ subst ls cod

dic "" _ = ""
dic cod listaTupla = [l | (l,c) <- listaTupla, cod == c]

dSubst [] _ = []
dSubst s cod = dSubst' s cod "" ""
    where
        dSubst' [] cod teste decod = decod ++ (dic teste cod)
        dSubst' (s:ss) cod teste decod   | (dic teste cod) /= "" = dSubst' (s:ss) (cod) ("") (decod ++ (dic teste cod))
                                         | otherwise             = dSubst' ss cod (teste ++ [s]) decod

--encode :: FilePath -> String
encode fileName = do
    h <- openFile fileName ReadMode
    l <- hGetLine h
    let cod = [('o',"000"),('a',"001"),('n',"010"),(' ',"011"),('d',"100"),('c',"1010"),('i',"1011"),('e',"1100"),('T',"11010"),('s',"110110"),('t',"110111"),('g',"11100"),('f',"11101"),('S',"11110"),('h',"111110"),('F',"111111")]
    return (subst l cod)

decode fileName = do
    h <- openFile fileName ReadMode
    l <- hGetLine h
    let cod = [('a',"00"),('n',"010"),('o',"011"),(' ',"1000"),('c',"1001"),('d',"1010"),('i',"1011"),('T',"11000"),('e',"11001"),('s',"11010"),('t',"11011"),('f',"11100"),('S',"11101"),('h',"11110"),('F',"11111")]
    return (dSubst l cod)

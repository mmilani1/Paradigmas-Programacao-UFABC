wordNumber c n = concat' $ intersperse' c [digiToStr x | x <- digitos n]

numeros = [ "zero", "um","dois","tres","quatro","cinco",
            "seis", "sete", "oito", "nove"]

digitos :: Int -> [Int]
digitos n = digitos' [] n
    where 
        digitos' resp n
            | n > 0     = digitos' (n`rem`10:resp) (n`div`10)
            | otherwise = resp

digiToStr :: Int -> [Char]
digiToStr n = head $ drop n numeros
     
concat' :: [[a]] -> [a]
concat' [[]]  = []
concat' [a] = a
concat' ass = concat'' [] ass
    where
        concat'' resp [[]]     = resp
        concat'' resp [a]    = resp++a
        concat'' resp (as:ass) = concat'' (resp++as) ass

intersperse' :: a -> [[a]] -> [[a]]
intersperse' _ [[]]   = [[]]
intersperse' _ [a] = [a]
intersperse' a (c:cs) = c : [a] : intersperse' a cs
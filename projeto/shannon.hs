import Data.List (sortBy)
import Data.Function (on)

mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` snd)

text :: String
text = "Este e um teste de obtencao de probabilidades"

comp :: Int
comp = length text

prob :: String -> Char -> (Char,Float)
prob s c
        | existe > 0    = (c, fromIntegral existe/fromIntegral comp)
        | otherwise     = (c,0.0)
    where
        existe = length (filter (==c) s)

probabilidades :: String -> [(Char,Float)]
probabilidades [] = []
probabilidades s = prob s (head s) : probabilidades (filter (/=head s) s)
import Test.QuickCheck
import Data.List

-- Decide se todos os valores logicos de uma lista sao True
and' :: [Bool] -> Bool
and' []          = True
and' (b:bs)
    | b == True = (and' bs)
    | otherwise = False

--Concatena uma lista de listas
concat' :: [[a]] -> [a]
concat' ((x:xs):xss) = x:(concat ((xs):xss))

--produz uma lista com n valores identicos
replic :: Int -> a -> [a]
replic 0 _ = []
replic n a = a:(replic (n-1) a)

--seleciona o enesimo elemento da lista
enes :: [a] -> Int -> a
enes [] _ = error "Invaildo"
enes (a:as) 0 = a
enes (a:as) n | n >= length (a:as) = error "Invalido"
              | otherwise = enes as (n-1)

ele :: Eq a => a -> [a] -> Bool
ele _ [] = False
ele a (b:bs) | a == b = True
             | otherwise = ele a bs

merge :: Ord a => [a] -> [a] -> [a]
merge [] as = as
merge as [] = as
merge (a:as) (b:bs)
        | a <= b    = a:(merge as (b:bs))
        | otherwise = b:(merge (a:as) bs)

metade :: [a] -> ([a],[a])
metade as = (a,b)
    where
        a = take (length as `div` 2) as
        b = drop (length as `div` 2) as


msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort as = merge (msort (fst (metade as))) (msort (snd (metade as)))

prop_idempotencia :: Ord a => [a] -> Bool
prop_idempotencia xs = msort (msort xs) == msort xs

prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs) ==> head (msort xs) == minimum xs

prop_model :: Ord a => [a] -> Bool
prop_model xs = msort xs == sort xs

prop_length :: Ord a => [a] -> Bool
prop_length xs = length xs == length (msort xs)


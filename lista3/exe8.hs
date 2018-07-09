merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] a = a
merge (a:as) (b:bs)
    | a < b    = a : merge as (b:bs)
    | otherwise = b : merge (a:as) bs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [a] = [a]
msort as = merge (msort $ fst $ halve as) (msort $ snd $ halve as)

halve :: [a] -> ([a],[a])
halve as = (m1,m2)
    where
        m1 = take (length as `div` 2) as
        m2 = drop (length as `div` 2) as
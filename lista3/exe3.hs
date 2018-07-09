euclid :: Int -> Int -> Int
euclid x y
    | x == y    = x
    | x > y     = euclid y (x-y)
    | otherwise = euclid x (y-x)   
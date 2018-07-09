penultimo :: Eq a => [a] -> a
penultimo as
    | as == []          = error "invalido"
    | length as == 1    = error "invalido"
    | otherwise         =  last (init as)
produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar as bs = sum m
    where
        m = [a * b | (a,b) <- zip as bs]
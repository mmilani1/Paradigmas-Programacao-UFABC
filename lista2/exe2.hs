maxL :: [Int] -> [Int]
maxL (a:b:c:ns)
    | b>a && b>c    = b:maxL (c:ns)
    | otherwise     = maxL (b:c:ns)
maxL _ = []
intersperse' :: a -> [a] -> [a]
intersperse' _ []   = []
intersperse' _ (a:[]) = a:[]
intersperse' a (c:cs) = c : a : intersperse' a cs
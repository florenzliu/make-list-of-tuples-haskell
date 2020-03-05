-- iterate elements in list b
iterateB :: a -> [b] -> [(a,b)]
iterateB _ [] = []
iterateB x (y:ys) = (x,y) : iterateB x ys

-- iterate elements in list a
func :: [a] -> [b] -> [(a,b)]
func [] _ = []
func (x:xs) ys = iterateB x ys ++ func xs ys

--func [1,2,3] [4,5]
-- iterate elements in list b
iterateB :: a -> [b] -> [(a,b)]
iterateB _ [] = []
iterateB x (y:ys) = (x,y) : iterateB x ys

-- iterate elements in list a
makeTuples :: [a] -> [b] -> [(a,b)]
makeTuples [] _ = []
makeTuples (x:xs) ys = iterateB x ys ++ makeTuples xs ys

--makeTuples [1,2,3] [4,5]
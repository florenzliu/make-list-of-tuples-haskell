-- Question 1. Find the maximum of a list of numbers.
findMax :: (Ord a) => [a] -> a
findMax [] = error "No maximum for the empty list"
findMax [x] = x
findMax (x:xs)
  | x > currMax = x
  | otherwise = currMax
  where currMax = findMax xs

-- Question 2. Check if the intersection of two given list exists.
-- find an element in a list
findElem :: (Eq a) => a -> [a] -> Bool
findElem _ [] = False
findElem x (y:ys) 
  | x == y = True
  | otherwise = findElem x ys
-- check if the intersection of two given list exists
hasIntersect :: (Eq a) => [a] -> [a] -> Bool 
hasIntersect _ [] = False
hasIntersect [] _ = False
hasIntersect (x:xs) ys 
  | findElem x ys = True
  | otherwise = hasIntersect xs ys

-- Question 3. Union of two given lists.
isFound :: (Eq a) => a -> [a] -> Bool
isFound _ [] = False
isFound x (y:ys) 
  | x == y = True
  | otherwise = isFound x ys

findUnion :: (Eq a) => [a] -> [a] -> [a]
findUnion xs [] = xs
findUnion [] ys = ys
findUnion xs (y:ys)
  | isFound y xs = findUnion xs ys
  | otherwise = [y] ++ findUnion xs ys

-- Question 4. Return the final element of a list.
lastElem :: (Ord a) => [a] -> a
lastElem [] = error "No final element in empty list"  
lastElem [x] = x 
lastElem (x:xs) = lastElem xs

-- Question 5. Return a list of tuples from mapping the elements of two lists.
-- iterate elements in list b
iterateB :: a -> [b] -> [(a,b)]
iterateB _ [] = []
iterateB x (y:ys) = (x,y) : iterateB x ys
-- iterate elements in list a
func :: [a] -> [b] -> [(a,b)]
func [] _ = []
func (x:xs) ys = iterateB x ys ++ func xs ys
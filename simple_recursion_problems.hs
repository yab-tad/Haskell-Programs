-- maximum function
maximum' :: (Ord a) => [a] -> a
maximum' [] = errorWithoutStackTrace "Empty list!"
maximum' [x] = x
maximum' (x:xs) = x `max` (maximum' xs)
  {-  | x > remainingMax = x
    | otherwise = remainingMax
    where remainingMax = maximum' xs -}

-- element replicating function
replicate' :: (Num b, Ord b) => b -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

-- take function reproduced
take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- a function to reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- a functions that produces a list with an infinite number of the same element
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip function reproduced
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- elem function reproduced
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs)
  | y == x = True 
  | otherwise = elem' y xs

-- Quick Sort function
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) =
  let smallerOrEq = [x | x <- xs, x <= p]
      larger = [x | x <- xs, x > p]
  in quickSort smallerOrEq ++ [p] ++ quickSort larger

-- Function that sorts and merges two lists
merge' :: (Ord a, Eq a) => [a] -> [a] -> [a]
merge' [] (y:ys) = (y:ys)
merge' (x:xs) [] = (x:xs)
merge' [] [] = []
merge' (x:xs) (y:ys)
  | x <= y = quickSort (x : merge' xs (y:ys))
  | otherwise = quickSort (y : merge' (x:xs) ys)
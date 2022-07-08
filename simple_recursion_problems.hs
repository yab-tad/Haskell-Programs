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
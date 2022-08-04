-- Working with curried functions
-- Creating a new function by calling a function with too few parameters

multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

{-
Now by calling multThree function with one parameter and setting it to a name we can create a partial function:

let multTwoThreeNine = multThree 9
multTwoThreeNine 2 3

OUTPUT: 54

Similarly, it can also be written as:
let multTwoThreeNine = multThree 9 2
multTwoThreeNine 3

OUTPUT: 54
-}

-- Comparing with 100
{-
The function `compare` takes in an Ord classtyped parameter and returns a function that accepts a parameter of the same type and returns an ordering function.

The function declaration below is the same as:

    compareWithHundred :: Int -> Ordering
    compareWithHundred x = compare 100 x
-}
compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

-- Working with sections to partially apply infix functions. (Infix function: + subtract * / > < == >= && ||)
-- Functions that do arithmetic operations with the number 10
divbyTen :: (Floating a) => a -> a
divbyTen = (/10)

mulbyTen :: (Floating a) => a -> a
mulbyTen = (*10)

addbyTen :: (Floating a) => a -> a
addbyTen = (+10)

subbyTen :: (Floating a) => a -> a
subbyTen = (subtract 10)

arithByTen :: (Floating a, Show a) => a -> String
arithByTen a =
    let d = "Division Result : " ++ show (divbyTen a)
        m = "Multiplication Result : " ++ show (mulbyTen a)
        ad = "Addition Result : " ++ show (addbyTen a)
        su = "Subtraction Result : " ++ show (subbyTen a)
    in d ++ " -- " ++ m ++ " -- " ++ ad ++ " -- " ++ su

-- a boolean function that checks whether its parameter is an Uppercase character or not
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Some higher order function that takes in a function of some type and a value of the same type as parameters and returns a value of the same type
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Implementation of the zipWith function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Fliping function
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- Map function
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Filter function. This function takes in predicates and returns a boolean value, and a list then return a list whose elements satisfy the predicate conditions
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

-- QuickSort using the filter function
quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerElements = quickSort' (filter' (<=x) xs)
        largerElements = quickSort' (filter' (>x) xs)
    in smallerElements ++ [x] ++ largerElements

-- a function that returns the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- implementing takeWhile function
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

-- sum of odd squared numbers less than 10,000
sOSQ :: Integer
sOSQ = sum (takeWhile' (<10000) (filter' odd (map (^2) [1..])))

-- Collatz Sequence
collatzSeq :: (Integral a) => a -> [a]
collatzSeq 1 = [1]
collatzSeq n
    | even n = n : collatzSeq (n `div` 2)
    | odd n = n : collatzSeq (n*3 + 1)

-- Collatz sequences of numbers from 1 to 100 (inclusive) that have length greater than 15
num15LongChains :: Int
-- num15LongChains = length (filter (>15) (map length (map collatzSeq [1..100])))
num15LongChains = length (filter len (map collatzSeq [1..100]))
    where len xs = length xs > 15

-- another interesting usecase of partial function using map
listOfMultipliers :: [Integer -> Integer]
listOfMultipliers = map (*) [0..]

multiple :: Integer -> Integer
multiple = (listOfMultipliers !! 2) 
{- now adding any integer as a parameter for the function(name) multiple 
multiplies the value by the second element of the multiple function returned 
by the function listOfMultipliers. -}

-- using lambda function to implement num15LongChains
num15LongChainsLambda :: Int
num15LongChainsLambda = length (filter (\xs -> length xs > 15) (map collatzSeq [1..100]))

-- more implementations of lambda function
zipMeReal :: [Double]
zipMeReal = zipWith (\a b -> (a * 30 + 3) / b)[5,4,3,2,1] [1,2,3,4,5]

mapMeReal :: [Integer]
mapMeReal = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- due to the way functions are curried by default, these two are equivalent:
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x+y+z

addThreeLambda :: (Num a) => a -> a -> a -> a
addThreeLambda = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x {- usually used in this case to indicate that our function 
is meant to be partially applied and passed on to a function as a parameter. -}

-- lambda expression implementation for using the function map for 2D Lists
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D _ [[]] = [[]]
map2D f xs = map (\ys -> map f ys) xs

-- map function for 2D Lists using function Composition
map2DCom :: (a -> b) -> [[a]] -> [[b]]
map2DCom f xs = (map . map) f xs

-- using the dollar sign ($) inorder to bring forth a cleaner code with less parenthesis
lenOfNewList :: Int
lenOfNewList = length $ map (+1) [1,2,3]

filteredMappedL :: (Num a, Ord a) => [a] -> [a]
filteredMappedL xs = map (^2) $ filter (>3) xs

-- dropWhile implementation
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p xs@(y:ys)
    | p y = dropWhile' p ys
    | otherwise = xs

-- concatinating 2D lists
concat2D :: [[a]] -> [a]
concat2D [] = []
concat2D (x:xs) = x ++ concat2D xs

-- sum of cubed elements of a list
cubedSum :: (Num a) => [a] -> a
cubedSum xs = sum $ map (^3) xs

{- Pack consecutive duplicates of list elements into sublists. 
If a list contains repeated elements they should be placed in separate sublists. -}
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack c@(x:xs)= adj : (pack . drop adjLen) c
    where adj = takeWhile(== x) c
          adjLen = length adj

-- Implementing Fold both for foldr (fold right) and foldl (fold left)
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

-- Building functions using fold

concat2Dr :: [[a]] -> [a]
concat2Dr xss = foldr' (++) [] xss

sumr :: Integral a => [a] -> a
sumr = foldr' (+) 0

concat2Dl :: [[a]] -> [a]
concat2Dl = foldl' (++) []

suml :: [Int] -> Int
suml xs = foldl' (+) 0 xs

and', or' :: [Bool] -> Bool
and' xs = foldl' (&&) True xs
or' xs = foldr' (||) False xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl' (\z y -> if x == y then True else z) False

mapR :: (a -> b) -> [a] -> [b]
mapR f xs = foldr' (\x acc -> f x : acc) [] xs

mapL :: (a -> b) -> [a] -> [b]
mapL f xs = foldl' (\acc x -> acc ++ [f x]) [] xs

filterR :: (Eq a, Ord a) => (a -> Bool) -> [a] -> [a]
filterR p xs = foldr' (\x acc -> if p x then x:acc else acc) [] xs

filterL :: (Eq a, Ord a) => (a -> Bool) -> [a] ->[a]
filterL p xs= foldl' (\acc x -> if p x then acc ++ [x] else acc) [] xs

takeR :: Int -> [a] -> [a]
takeR n xs = foldr' (\x container -> if length container < n then container ++ [x] else container) [] (reverse xs)

takeL :: Int -> [a] -> [a]
takeL n xs = reverse(foldl' (\acc x -> if length acc < n then x:acc else acc ) [] xs)

{- By using foldl1 and foldr1 we eliminate the need to provide them with an 
explicite starting value as they treat the first (or last) element of the 
list as a starting value and then start folding it with the element next to it -}

-- implementing the sum function
sumL1 :: [Int] -> Int
sumL1 = foldl1 (+)

sumR1 :: [Int] -> Int
sumR1 = foldr1 (+)

headR1 :: [a] -> a
headR1 = foldr1 (\x _ -> x)

lastL1 :: [a] -> a
lastL1 = foldl1 (\_ x -> x)

productR1:: [Int] -> Int
productR1 = foldr1 (*)

productL1 :: [Int] -> Int
productL1 = foldl1 (*)

-- maximum function
maximumR1 :: Ord a => [a] -> a
maximumR1 = foldr1 (\x acc -> if x > acc then x else acc)

maximumL1 :: Ord a => [a] -> a
maximumL1 = foldl1 (\acc x -> if x > acc then x else acc)

-- reverse function
reverseR :: [a] -> [a]
reverseR xs = foldr' (\x acc -> acc ++ [x]) [] xs

reverseL :: [a] -> [a]
reverseL = foldl' (\acc x -> x : acc) []

-- scanl and scanr are like foldl and foldr, but they report on the intermediate accumulator states

accStateAddL :: [Integer]
accStateAddL = scanl (+) 0 [1,2,3,4] -- returns [0,1,3,6,10]

accStateAddR :: [Integer]
accStateAddR = scanr (+) 0 [1,2,3,4] -- returns [10,6,3,1,0]

flipMinus :: [Integer]
flipMinus = scanl (flip (-)) 2 [1,3,4,5]  --returns [2,-1,4,0,5]

flipConcat = scanl (flip (:)) [] [4,3,2,1] --returns [[],[4],[3,4],[2,3,4],[1,2,3,4]]

-- there also exists the possibility to use scanl1 and scanr1 counterparts

flipDiv1 :: [Double] -> [Double]
flipDiv1 = scanl1 (flip (/)) -- when fed [1,3,2,1], it returns [1.0,3.0,0.6666666666666666,1.5]

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000? 
sqrtSum :: Int
sqrtSum = (+1) . length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]
-- a more readable way to implement this is:
sqrtSum2 :: Int
sqrtSum2 = 
    let summedSqrt = scanl1 (+) . map sqrt $ [1..]
        lenLimit = length . takeWhile (<1000) $ summedSqrt
    in  lenLimit + 1
{- returns 131 as the summation of the square root of 131 elements results 
in a value greater than or equal to 10,000 -}
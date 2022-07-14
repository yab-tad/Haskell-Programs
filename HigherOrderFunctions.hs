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
zipMeRealGood :: [Double]
zipMeRealGood = zipWith (\a b -> (a * 30 + 3) / b)[5,4,3,2,1] [1,2,3,4,5]

mapMeRealGood :: [Integer]
mapMeRealGood = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

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
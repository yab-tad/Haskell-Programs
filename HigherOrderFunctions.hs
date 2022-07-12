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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Time.Format.ISO8601 (yearFormat)
{-# HLINT ignore "[]" #-}
{-# HLINT ignore "[]" #-}
doubleMe :: Num a => a -> a
doubleMe x = 2*x
doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2
quadrupleMe :: Num a => a -> a
quadrupleMe x = doubleMe x * 2

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = do
    if x > 100
    then x
    else x*2

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x = doubleSmallNumber x + 1

conanO'Brien :: [Char]
conanO'Brien = "Conan O'Brien"


mulOfThreeOneToTen :: [Integer]
mulOfThreeOneToTen = [x*3 | x <- [1..10]]

mulOfThreeGE12 :: [Integer]
mulOfThreeGE12 = [x*3 | x <- [1..10], x*3 >= 12]

-- Numbers from 50 to 100 whose remenders are 3 when devided by 7
rem3 :: [Integer]
rem3 = [x | x <- [50..100], x `mod` 7 == 3]

-- Boom if x < 10, and Bang if greater
boomBangs :: Integral a => [a] -> [[Char]]
boomBangs xs = [if x < 10 then "Boom" else "Bang" | x <- xs, odd x]

-- Numbers between 10 and 20 that are not 13, 15, and 19
picky :: [Integer]
picky = [x | x <- [10 .. 20], x /= 13, x /= 15, x /= 20]

-- All possible combinations problem
allCombo :: [Integer]
allCombo = [x*y| x <- [2,3,4], y <- [7,8,9], x*y < 25]

-- Combining a list of nouns with a list of adjectives
nouns :: [[Char]]
nouns = ["hobo", "frog", "pop"]
adjectives :: [[Char]]
adjectives = ["lazy", "grouchy", "scheming"]
nounAdj :: [[Char]]
nounAdj = [adjective ++ " " ++ noun | noun <- nouns, adjective <- adjectives]
nounAdj2 :: [([Char], [Char])]
nounAdj2 = zip adjectives nouns

-- length of list function
length' :: Num a => [t] -> a
length' lst = sum[1 | _ <- lst]

-- get the last element index of a list
lastElIndex :: [Integer] -> Float
lastElIndex lst = fromIntegral(length' lst) - 1.0

-- uppercase and lowercase remover
removeUppercase :: [Char] -> [Char]
removeUppercase str = [u | u <- str, u `elem` ['a' .. 'z']]
removeLowercase :: [Char] -> [Char]
removeLowercase str = [l | l <- str, l `elem` ['A' .. 'Z']]

-- odd and even number removal from nested lists
xxs :: [[Integer]]
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
oddRemoveNested :: [[Integer]]
oddRemoveNested = [[x | x <- xs, even x] | xs <- xxs]
evenRemoveNested :: [[Integer]]
evenRemoveNested = [[x | x <-xs, odd x] | xs <- xxs]

-- triangle an right triangle
triangles :: [(Integer,Integer, Integer)]
triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
rightTriangles :: [(Integer,Integer,Integer)]
rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTrianglesWithCondition :: [(Integer,Integer,Integer)]
rightTrianglesWithCondition = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]

-- type defination for a function that takes in three numbers and sums them
threeSum :: Int -> Int -> Int -> Int
threeSum x y z = x+y+z

-- factorial
factorial :: Integer -> Integer
factorial n = product[1..n]

-- circumference of a circle
circumference :: Float -> ([Char], Float)
circumference radius = ("Circumference is: ", 2 * pi * radius)

-- area of a circle
area :: Double -> ([Char], Double)
area radius = ("Area is: ", pi * radius^2)

-- pattern matching trial
lucky :: (Integral a) => a -> String
lucky 0 = "Zero"
lucky 1 = "One"
lucky 2 = "Two"
lucky 3 = "Three"
lucky x = "Better luck next time ;)"

-- factorial in haskell ways
factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * factorial' (n-1)

-- vector addition
addVector :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
addVector (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

-- extracting components from triple pairs
first :: (a,b,c) -> a
first (a,_,_) = a
second :: (a,b,c) -> b
second (_,b,_) = b
third :: (a,b,c) -> c
third (_,_,c) = c

-- pattern matching on list comprehension
xs1 :: [[(Integer, Integer)]]
xs1 = [[(1,2), (3,4), (5,6), (7,8)],[(9,10), (11,12), (13,14)]]
pairSum :: [[Integer]]
pairSum = [[a+b | (a,b) <- xs2] | xs2 <- xs1]

-- head function
head' :: [a] -> a
head' [] = errorWithoutStackTrace "Can't extract from empty list"
head' (x:_) = x

-- list description
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list has more than two elements. The first two are: " ++ show x ++ " and " ++ show y

-- finding length of list resurssively
length'' :: Num b => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' (xs)

-- summing elements of a list
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xr) = (x) + sum' (xr)

-- pattern binding
capital :: String -> String
capital "" = "Empty string!"
capital all@(x:xs) = "The first element of " ++ all ++ " is " ++ [x]

--bmiTell
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Underweight!"
    | bmi <= normal = "Normal!"
    | bmi <= fat = "Overweight!"
    | otherwise = "God help you!" 
    where bmi = weight / height^2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- max redefined
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

-- compare restructured
compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- creating initials
initial :: String -> String -> String
initial firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname 

-- a bunch of BMIs
calcBMIs :: (RealFloat a) => [(a,a)] -> [[Char]]
calcBMIs xs = [bmiTell w h | (w,h) <- xs]

-- using let in
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let surface_area = 2 * pi * r * h
        base_area = pi * r^2
    in 2 * base_area + surface_area

-- Greeting agent 47
greet :: ([Char], [Char])
greet = (let g = "Greetings "; a = "Agent " in g ++ a, let (fh, fs)=("#",47) in fh ++ show fs)

-- pythagorean theorem using let
pythagoras :: RealFloat a => [(a, a)] -> [a]
pythagoras xs = [hyp | (a,b) <- xs, let hyp = a^2 + b^2, hyp > 35]

-- head function using case of
head'' :: [a] -> a
head'' xs = case xs of
                [] -> error"Empty list"
                (x:_) -> x

-- describing a list's number of elements
describeL :: Show a => [a] -> String
describeL xs = "This list has " ++ case xs of [] -> "no elements -- empty"
                                              [x] -> "one element and that is: " ++ show x
                                              xs -> show (length'' xs) ++ " elements!"

-- another way to write express this is
describeL' :: Show a => [a] -> [Char]
describeL' xs = "This list " ++ what xs
    where what [] = "is empty!"
          what [x] = "contains a single element, which is: " ++ show x
          what xs = "contains " ++ show (length'' (xs)) ++ " elements!"

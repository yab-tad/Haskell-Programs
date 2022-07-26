import Data.List

-- number of unique elements in a list
numUnq :: Eq a => [a] -> Int
numUnq = length . nub  --given input [1,1,3,2,4,2,1,4] it returns 4.

{-
    If you just need a couple of functions from a module, you can selectively import just 
    those functions. If we wanted to import only the nub and sort functions from Data.List, 
    we'd do this:
                
                import Data.List (nub, sort)


    You can also choose to import all of the functions of a module except a few select ones. 
    That's often useful when several modules export functions with the same name and you want 
    to get rid of the offending ones. Say we already have our own function that's called nub 
    and we want to import all the functions from Data.List except the nub function:
                
                import Data.List hiding (nub)

    Another way to counter name clashes is to use |qualified imports|. For example, when 
    we import Data.Map and try to use the filter function, it gets confused as to which 
    filter to use since the function's name from the Data.Map is the same as the Prelude 
    module. A solution for this looks like qualifying the import and calling the function 
    by appending the function we want with the imported module.
    This looks like the following:

                import qualified Data.Map
        
        Calling the function filter from the imported module now would look like: Data.Map.filter


    Assigning shorthand notations for the modules we import can further help create a 
    smoothe process. It specially comes very handy when a cascading import is at play:

                import qualified Data.Map as M

        Now, things become a lot simpler and clearer: M.filter

-}

-- Some important functions form Data.List are as follows:

{- `intersperse` : takes an element and a list and puts it in 
   between each pair of elements in the list -}
intersperseTest :: [Char]
intersperseTest = intersperse '.' "MONKEY" -- returns "M.O.N.K.E.Y"

-- `intercalate` : takes a list and a list of list and inserts 
-- it between each inner list and flattens the result
intercalateTest1 :: [Integer]
intercalateTest1 = intercalate[0,0,0] [[1,1,1],[2,2,2],[3,3,3]] --returns [1,1,1,0,0,0,2,2,2,0,0,0,3,3,3]

intercalateTest2 :: [Char]
intercalateTest2 = intercalate " " ["Space", "is", "awsome!"] --returns "Space is awsome!"

-- `transpose` : takes in a list of list and transposes it, meaning 
-- the rows of the two-dimensional matrix become the columns and vice versa.
transposeTest1 :: [[Integer]]
transposeTest1 = transpose [[1,2,3],[4,5,6],[7,8,9]] -- returns [[1,4,7],[2,5,8],[3,6,9]]

transposeTest2 :: [[Char]]
transposeTest2 = transpose ["hey", "there", "gals"] --returns ["htg","eha","yel","rs","e"]

{- Say we have the polynomials 3x2 + 5x + 9, 10x3 + 9 and 
8x3 + 5x2 + x - 1 and we want to add them together. We can use 
the lists [0,3,5,9], [10,0,0,9] and [8,5,1,-1] to represent them 
in Haskell. Now, to add them, we can make use of the transpose function. -}

transposePoly :: [Int]  -- returns [18,8,6,17]
transposePoly = map sum $ transpose [[0,3,5,9], [10,0,0,9], [8,5,1,-1]]

-- `concat` : flattens a list of lists into just a single list concatenating the inner lists
concatTest1 :: [Int]
concatTest1 = concat [[1,2,3],[4,5,6],[7,8,9]] --return [1,2,3,4,5,6,7,8,9]

concatTest2 :: String
concatTest2 = concat ["hello ", "everyone", "!"] -- returns "hello everyone!"

-- `concatMap` : maps a function to the list and concatenates it
concatMapT1 :: [Int]
concatMapT1 = concatMap reverse [[1,2,3],[4,5,6]] --returns [3,2,1,6,5,4]

concatMapT2 :: [Int]
concatMapT2 = concatMap (replicate 3) [1,2,3] --returns [1,1,1,2,2,2,3,3,3]

concatMapT3 :: [Int] --returns [1,1,1,2,2,2,3,3,3]
concatMapT3 = concat $ concatMap (replicate 3) [[1],[2],[3]]

-- `and` : takes a list of boolean values and returns True only when all the elements of the list return True
andT1 :: Bool
andT1 = (and . map (>2)) [3,4,5,6] -- returns True

andT2 :: Bool
andT2 = and . map (==3) $ [1,2,3] -- returns False

-- `or` : is like `and` but returns True if at least one of the elements in the list is True
orT :: Bool
orT = or . map (<1) $ [0,1,3] --returns True

-- `any` and `all` : they check whether any or all the elements of a list satisfy the given conditon, respectively.
anyT1 :: Bool
anyT1 = any (>3) [1,2,3,4] -- retuns True

allT1 :: Bool
allT1 = all (==5) [5,5,3,6,2] --returns False

anyT2 :: Bool
anyT2 = any (`elem` ['A'..'Z']) "fLAvored Popcorn Is cOOl" --returns True

allT2 :: Bool
allT2 = all (`elem` ['a'..'z']) "boysgrowtobecomemen" --returns True

{- `iterate` : it takes a function and starting value. It applies 
the funciton to the starting value, and then to the result and so on. -}
iterT1 :: [Int]
iterT1 = (take 10 . iterate (*2)) 1 --returns [1,2,4,8,16,32,64,128,256,512]

iterEvilLaugh :: [[Char]] --returns ["ha","hahaha","hahahahaha","hahahahahahaha"]
iterEvilLaugh = take 4 $ iterate (++"haha") "ha"

{- `splitAt` : takes in a number and list, and splits the elements 
of the list with the same amount of number it took and returns 
two lists in a tuple. -}
splitAtT1 :: ([Char], [Char])
splitAtT1 = splitAt 3 "what's up" --returns ("wha","t's up")

splitAtT2 :: ([Char], [Char]) --returns ("well hello children","")
splitAtT2 = splitAt 34 "well hello children"

splitAtT3 :: [Char]  --returns "courtfood"
splitAtT3 = 
        let (a,b) = splitAt 4 "foodcourt"
        in b ++ a

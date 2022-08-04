import Data.List
import qualified Data.Map as Map
import Data.Maybe()
import Data.Char
import InteractiveEval (Term(val))

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

splitAtT3 :: ([Char], [Char])
splitAtT3 = splitAt (-2) "hello" --returns ("","hello")

splitAtT4 :: [Char]  --returns "courtfood"
splitAtT4 = 
        let (a,b) = splitAt 4 "foodcourt"
        in b ++ a

{- `takeWhile` : takes a predicate and a list. It parses through the 
list until an element is encountered that doesn't satisfy the predicate 
conditoin. It then returns the previous elements in a list -}

-- Say we wanted to know the sum of all third powers that are under 10,000.
sumCubedP :: Integer
sumCubedP = sum . takeWhile (<10000) $ map (^3) [1..] --returns 53361

{- `dropWhile` : similar to the `takeWhile` function but instead of returning 
the list until the predicate is broken, `dropWhile` drops the elements that 
satisfy the predicate and return the rest once the condition is broken. -}
dropWtest1 :: [Int]
dropWtest1 = dropWhile (==3) [3,3,3,1,2,3,4] -- returns [1,2,3,4]

{- We're given a list that represents the value of a stock by date. The 
list is made of tuples whose first component is the stock value, the 
second is the year, the third is the month and the fourth is the date. We 
want to know when the stock value first exceeded one thousand dollars! -}

dropWtest2 :: (Double, Integer, Integer, Integer) --returns (1001.4,2008,9,4)
dropWtest2 = 
        let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
        in head $ dropWhile (\(v,y,m,d) -> v < 1000) stock

{- `span` : is like `takeWhile` but returns a pair of lists in a tuple. The first contains the 
elements satisfying the predicate and the second contains the rest of the elements. -}
spanT1 :: [Char]
spanT1 = 
        let (first, rest) = span (/= ' ') "The one who shall not be named"
        in "First: " ++ first ++ " | rest:" ++ rest

{- `break` : similar to `span`, but instead of spanning through the list 
when the predicate is met it breaks and return the elements until then as 
the first list and the rest on the second. -}

breakT1 :: [Char] --returns "First: , rest:The one who shall not be named"
breakT1 = let (first, rest) = break (/= ' ') "The one who shall not be named"
          in "First: " ++ first ++ ", rest:" ++ rest

breakT2 :: ([Int],[Int])
breakT2 = break (>3) [1,2,3,4,5,6] --return ([1,2,3],[4,5,6])

{- `sort` : simply sorts a list. The type of the elements in the list has 
to be part of the Ord typeclass, because if the elements of a list can't 
be put in some kind of order, then the list can't be sorted. -}

sortT1 :: [Integer]
sortT1 = sort [31,4,5,62,44,6] -- returns [4,5,6,31,44,62]

sortT2 :: [Char]
sortT2 = sort "haskell is lazy"  --returns "  aaehiklllssyz"

-- `group` : takes a list and groups adjecent elements in sublist if they are equal
groupT1 :: [[Integer]]
groupT1 = group [1,1,3,3,3,4,2,2,2,3,3,5] --returns [[1,1],[3,3,3],[4],[2,2,2],[3,3],[5]]

groupT2 :: [[Char]]  --returns ["aa"," ","bbb"," ","a"," ","aaa"," ","bb"," ","ccc"," ","aa"]
groupT2 = group "aa bbb a aaa bb ccc aa"

-- If we sort a list before grouping it, we can find out how many times each element appears in the list.
groupT3 :: [(Int, Int)] --returns [(1,6),(2,4),(3,4)]
groupT3 = map (\l@(x:xs) -> (x,length(l))) . group . sort $ [1,1,1,1,2,2,2,3,3,1,1,3,3,2]

-- `inits` and `tails` : they are like `init` and `tail` but they recursively go through every element in the list.
initails :: [([Char],[Char])]
initails = let l = "hello"  --returns [("","hello"),("h","ello"),("he","llo"),("hel","lo"),("hell","o"),("hello","")]
           in zip (inits l) (tails l)

search :: Eq a => [a] -> [a] -> Bool
search needle haystack = 
        let nlen = length needle
        in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- `isInfixOf` : similar to the `search` function defined above
isInf1 :: Bool
isInf1 = isInfixOf "cat" "I own a cat" --returns True

isInf2 :: Bool
isInf2 = "Cat" `isInfixOf` "a cat hunter" --returns False

-- `isPrefixOf` and `isSuffixOf` : search for a sublist at the beginning and at the end of a list, resectively.
isPreT :: Bool
isPreT = "cat" `isPrefixOf` "cats are adorable" -- returns True

isSufT1 :: Bool
isSufT1 = "cat" `isSuffixOf` "i don't have cats" --returns False

isSufT2 :: Bool
isSufT2 = "cat" `isSuffixOf` "i don't have a cat" --returns True

-- `elem` and `notElem` : to check if an element is inside a list or not
e1 :: Bool
e1 = elem 3 [1,2,3] --returns True

e2 :: Bool
e2 = notElem 3 [1,2,5] --returns True

-- `partition` : takes a list and a predicate and returns a pair of lists. The first list in the result contains all the elements that satisfy the predicate, the second contains all the ones that don't.
partT :: ([Char], [Char]) --returns ("MDS","y name is r. trange")
partT = partition (`elem` ['A'..'Z']) "My name is Dr. Strange"

-- `find` : takes a list and a predicate and returns the first element that satisfies the predicate. But it returns that element wrapped in a Maybe value.
--find' :: (a -> Bool) -> [a] -> Maybe a
--find' f xs = listToMaybe $ filter f xs

find1 :: Maybe Integer
find1 = find (==3) [1,2,3,4,5] --returns Just 3

find2 :: Maybe Integer
find2 = find (>6) [1,2,3,4,5] --returns Nothing

-- Maybe datatype definition
-- data Maybe k = Nothing | Just k

-- Division using Maybe
safeDiv :: (Integral a) => a -> a -> Maybe a
safeDiv a b = if b == 0 then Nothing else Just $ div a b

----- Data.Map

-- phone book dictionary
phoneBook :: [([Char], [Char])]
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")
    ,("penny","856-4229")  
    ]

-- Key-Value pair matcher in a dictionary
dictFind :: Eq key => key -> [(key, value)] -> Maybe value
dictFind key dict = foldl (\acc (k,v) -> if k==key then Just v else acc) Nothing dict
{- dictFind key dict = snd . head . filter (\(k,v) -> (key==k)) $ dict

 dictFind key [] = Nothing
 dictFind key ((k,v):xs) = if k == key then Just v else dictFind key xs -}

phoneBookToMap1:: Ord k => [(k,String)] -> Map.Map k String
phoneBookToMap1 xs = Map.fromListWith (\num1 num2 -> num1 ++ ", " ++ num2) xs 
-- for [("ab","kv"),("ab","kd")] it returns fromList [("ab","kd, kv")]


penny1 :: Maybe String
penny1 = Map.lookup "penny" $ phoneBookToMap1 phoneBook -- returns Just "856-4229, 853-2492"


phoneBookToMap2 :: Ord k => [(k,String)] -> Map.Map k [String] 
phoneBookToMap2 xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
-- for [("ab","kv"),("ab","kd")]) it returns fromList [("ab",["kd","kv"])]


penny2 :: Maybe [String]
penny2 = Map.lookup "penny" $ phoneBookToMap2 phoneBook -- returns Just ["856-4229","853-2492"]

addSimKey :: (Ord k, Num v) => [(k,v)] -> Map.Map k v
addSimKey = Map.fromListWith (+)
-- given [(1,2),(1,4),(2,4),(3,9),(3,1)] it returns fromList [(1,6),(2,4),(3,10)]

mapit :: (Ord k, Num a) => a -> [(k,a)] -> Map.Map k a
mapit m xs = Map.map (*m) $ Map.fromList xs
-- Given 100 [(1,1),(2,2),(3,3)] it returns fromList [(1,100),(2,200),(3,300)]

-- `singleton` takes a key and a value and creates a map that has exactly one mapping.
-- 'toList` is the inverse of `fromList`
mapToList :: [(Integer, Integer)]
mapToList = Map.toList $ Map.insert 8 6 $ Map.singleton 4 3 --returns [(4,3),(8,6)]

-- filterUpp :: Map.Map k v
filterUpp :: Ord k => [(k,Char)] -> Map.Map k Char
filterUpp xs = Map.filter isUpper $ Map.fromList xs
-- Given [(1,'a'),(2,'A'),(3,'b'),(4,'B')] it returns fromList [(2,'A'),(4,'B')]

insertW :: Ord a => (a -> a -> a) -> a -> a -> [(a,a)] -> Map.Map a a
insertW f k v xs = Map.insertWith f k v $ Map.fromList xs
-- addInsertW (*) 3 100 [(3,200),(4,400)] returns fromList [(3,20000),(4,400)]
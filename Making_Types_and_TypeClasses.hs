
-- Exporting functions and types to a module
-- module Shapes (
--     Shape'(..),
--     Point'(..),
--     baseCircle,
--     baseRect,
--     nudge,
--     surface
-- ) where


import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Read (readMaybe)
import GHC (typecheckModule)
import Control.Monad.Trans.Writer (Writer)


-- Algebraic Data types
data Bool' = True | False

-- data Int' = (-2147483648) | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647 


-- One way to define our type is as follows:
-- data Shape' = Circle Float Float Float | Rectangle Float Float Float Float deriving(Show)

-- Another way is to use additional value constructor for our value constructors
data Point' = Point' Float Float deriving (Show)
data Shape' = Circle Point' Float | Rectangle Point' Point' deriving (Show)

-- Circle and Rectangle are value constructors which take 3 and 4 fields (parameters), respectively.
-- checking the type of Circle and Rectangle give us:
--          Circle :: Point' -> Float -> Shape'
--  Where it would have a type of  (Circle :: Float -> Float -> Float -> Shape') if we hadn't used Point'.
--  
--  The same goes for Rectangle. It would have returned a type of (Rectangle :: Float -> Float -> Float -> Float -> Shape')
--          Rectangle :: Point' -> Point' -> Shape'

{- If we haven't defined `Shape'` by deriving the `Show` into it, Circle and Rectangle won't have the functionality to be displayed on their own.
   For example, passing on (Circle 10 20 5) one ghci would give us (Circle 10.0 20.0 5.0), which became possible because we derived `Show` over it.
   And passing (map (Circle 10 20) [4,5,6,6]) returns ([Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]). -}



surface :: Shape' -> Float
surface (Circle _ r) = pi Prelude.* r^2
surface (Rectangle (Point' x1 y1) (Point' x2 y2)) = (Prelude.abs $ x2 Prelude.- x1) Prelude.* (Prelude.abs $ y2 Prelude.- y1)

-- Passing (surface $ Circle (Point' 0 0) 24) returns 1809.5574
-- Passing (surface $ Circle (Point' 0 0) 24) returns 150.0

-- Creating a transposing function where it takes the coordinates of `Shape'` value constructures and gives a new coordinate based on the specifications given to it.
nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle (Point' x y) r) a b = Circle (Point' (x Prelude.+ a) (y Prelude.+ b)) r
nudge (Rectangle (Point' x1 y1) (Point' x2 y2)) a b = Rectangle (Point' (x1 Prelude.+ a) (y1 Prelude.+ b)) (Point' (x2 Prelude.+ a)(y2 Prelude.+ b))

-- Passing (nudge (Circle (Point' 0 0) 4) 2 3) returns Circle (Point' 2.0 3.0) 4.0
-- Passing (nudge (Rectangle (Point' 0 0) (Point' 2 3)) 10 10) returns Rectangle (Point' 10.0 10.0) (Point' 12.0 13.0)

{- If we don't want to deal directly with points, we can make some auxilliary functions that 
create shapes of some size at the zero coordinates and then nudge those. -}
baseCircle :: Float -> Shape'
baseCircle r = Circle (Point' 0 0) r

baseRect :: Float -> Float -> Shape'
baseRect width length = Rectangle (Point' 0 0) (Point' width length)


-- passing (nudge (baseCircle 4) 10 10) returns Circle (Point' 10.0 10.0) 4.0
-- passing (nudge (baseRect 2 3) 10 10) returns Rectangle (Point' 10 10) (Point' 12 13)


-- RECORD SYNTAX

-- a primitive way to define a data type where its components are unreadable and clunky
data Person' = Person' String String Int Float String String deriving (Show)
-- defining using Record Syntax
data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    specialty :: String
} deriving (Show)

guy1 :: Person'
guy1 = Person' "Thomas" "Edison" 53 1.63 "000-000-00-000" "Stealing"

-- calling guy1 on ghci returns Person' "Thomas" "Edison" 53 1.63 "000-000-00-000" "Stealing"

guy2 :: Person
guy2 = Person {firstName="Nikola", lastName="Tesla", age=48, height=1.78, phoneNumber="111-111-11-111", specialty="Inventing"}

-- calling guy2 on ghci returns Person {firstName = "Nikola", lastName = "Tesla", age = 48, height = 1.78, phoneNumber = "111-111-11-111", specialty = "Inventing"}

-- TYPE PARAMETERS
{- A value constructor can take some values parameters and then produce a 
new value. For instance, the Car constructor takes three values and produces 
a car value. In a similar manner, type constructors can take types as 
parameters to produce new types. -}

data Maybe' a = Nothing | Just a

data Car a b c = Car {company :: a, model :: b, year :: c} deriving(Show)

tellCar :: Show a => Car String String a -> String
tellCar (Car{company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ Prelude.show y

stang :: Car [Char] [Char] Int
stang = Car {company="Ford", model = "Mustang", year=1964}

tell_stang :: String
tell_stang = tellCar stang  --returns "This Ford Mustang was made in 1964"


data Vector a = Vector a a a deriving (Show)

vplus :: Num t => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector a b c) = Vector (i+a) (j+b) (k+c)

vectMult :: Num t => Vector t -> Vector t -> Vector t
(Vector i j k) `vectMult` (Vector a b c) = Vector (i*a) (j*b) (k*c)

scalarMult :: Num t => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector a b c) = (i*a) + (j*b) + (k*c)


-- DERIVED INSTANCES

data Person1 = Person1 {
    firstName' :: String,
    lastName' :: String,
    age' :: Int
} deriving (Eq, Show, Read)


mikeD :: Person1
mikeD = Person1 {firstName' = "Michael", lastName' = "Diamond", age' = 43}  
adRock :: Person1
adRock = Person1 {firstName' = "Adam", lastName' = "Horovitz", age' = 41}  
mca :: Person1
mca = Person1 {firstName' = "Adam", lastName' = "Yauch", age' = 44}

{- Checking for equality of the following give:
        mikeD /= adRock returns True
        mikeD == mca returns False
        adRock == mca returns False
-}

beastieBoys :: [Person1]
beastieBoys = [mca, adRock, mikeD]

checkBeastieBoys :: Bool -- returns True
checkBeastieBoys = Person1 {firstName' = "Michael", lastName' = "Diamond", age' = 43} `elem` beastieBoys

-- (read "Person1 {firstName' = \"Adam\", lastName' = \"Yauch\", age' = 44}" :: Person1) == mca
-- read "Person1 {firstName' = \"Adam\", lastName' = \"Yauch\", age' = 44}" == mca

{-
        data Bool = False | True deriving (Ord)  

Because the False value constructor is specified first and the True value 
constructor is specified after it, we can consider True as greater than False.
        True `compare` False returns GT
        True > False returns True

Similarily, In the Maybe a data type, the Nothing value constructor is specified 
before the Just value constructor, so a value of Nothing is always smaller 
than a value of Just something, even if that something is minus one billion trillion. 
But if we compare two Just values, then it goes to compare what's inside them.

        Nothing < Just 100 returns True  
        Nothing > Just (-49999) returns False  
        Just 3 `compare` Just 2 returns GT  
        Just 100 > Just 50 returns True 
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Show, Ord, Read, Eq, Enum, Bounded)
--Because all the value constructors are nullary (take no parameters, i.e. fields), we can make it part of the Enum typeclass.
{-
        ghci> Wednesday  
        Wednesday  
        ghci> show Wednesday  
        "Wednesday"  
        ghci> read "Saturday" :: Day  
        Saturday  

        ghci> Saturday == Sunday  
        False  
        ghci> Saturday == Saturday  
        True  
        ghci> Saturday > Friday  
        True  
        ghci> Monday `compare` Wednesday  
        LT  

        ghci> minBound :: Day  
        Monday  
        ghci> maxBound :: Day  
        Sunday

        ghci> succ Monday  
        Tuesday  
        ghci> pred Saturday  
        Friday  
        ghci> [Thursday .. Sunday]  
        [Thursday,Friday,Saturday,Sunday]  
        ghci> [minBound .. maxBound] :: [Day]  
        [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]   
-}

data Pokemon = Pokemon {
    pokedexId :: Int,
    name :: String,
    pokemonType :: [String],
    abilities :: [String]
} deriving (Show)

instance Eq Pokemon where
    pokemon1 == pokemon2 = pokedexId pokemon1 == pokedexId pokemon2

instance Ord Pokemon where
    pokemon1 <= pokemon2 = pokedexId pokemon1 <= pokedexId pokemon2
    pokemon1 > pokemon2 = pokedexId pokemon1 > pokedexId pokemon2


chansey :: Pokemon
chansey = Pokemon {pokedexId = 113, name = "Chansey", pokemonType = ["Normal"], abilities = ["Natural Cure", "Serene Grace"]}
slowking :: Pokemon
slowking = Pokemon {pokedexId = 199, name = "Slowking", pokemonType = ["Water", "Psychic"], abilities = ["Oblivious", "Own Tempo"]}
jigglypuff :: Pokemon
jigglypuff = Pokemon {pokedexId = 39, name = "Jigglypuff", pokemonType = ["Normal", "Fairy"], abilities = ["Cute Charm", "Competitive"]}

unsorted :: [Pokemon]
unsorted = [chansey, slowking, jigglypuff]
sorted :: [Pokemon]
sorted = sort [chansey, slowking, jigglypuff]

{-
        sorted == unsorted returns False
        sorted /= unsorted returns True
        sorted < unsorted returns True
-}


-- TYPE SYNONYMS
{- Type synonyms don't really do anything per se, they're just about giving some types different names so 
that they make more sense to someone reading our code and documentation. Here's how the standard library 
defines String as a synonym for [Char]. -}

-- type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook1 :: PhoneBook
phoneBook1 =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")
    ,("penny","856-4229")  
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name, pnum) `elem` pbook
-- inPhoneBook "wendy" "939-8282" phoneBook1 returns True

{- Type synonyms can also be parameterized. If we want a type that represents an association list type 
but still want it to be general so it can use any type as the keys and values, we can do this: -}
type AssocList k v = [(k,v)]  -- read "[(1,2),(3,5),(8,9)]" :: AssocList Int Int returns [(1,2),(3,5),(8,9)]

-- Partially applied type constructor
type IntMap v = Map.Map Int v
--Or
type IntMap' = Map.Map Int


-- data Either a b = Left a | Right b deriving(Show, Eq, Ord, Read)
{- It has two value constructors. If the Left is used, then its contents are of type a and if Right is used, 
   then its contents are of type b. So we can use this type to encapsulate a value of one type or another and then when we get a 
   value of type Either a b, we usually pattern match on both Left and Right and we different stuff based on which one of them it was. -}

{- 
    An example: a high-school has lockers so that students have some place to put their Guns'n'Roses posters. Each locker has a 
    code combination. When a student wants a new locker, they tell the locker supervisor which locker number they want and he 
    gives them the code. However, if someone is already using that locker, he can't tell them the code for the locker and they 
    have to pick a different one. We'll use a map from Data.Map to represent the lockers. It'll map from locker numbers to a 
    pair of whether the locker is in use or not and the locker code.
-}

data LockerState = Taken | Free deriving(Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
                                    Prelude.Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " does not exists!"
                                    Prelude.Just(state, code) -> if state == Free 
                                                            then Right code
                                                            else Left $ "Locker number " ++ show lockerNumber ++ " is taken!" 

lockers :: LockerMap
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 



-- RECURSIVE DATA STRUCTURES

-- Implementing the List data type
data List a = Empty | Cons a (List a) deriving (Show, Eq, Ord, Read)
-- data List a = Empty | Cons {listHead :: a, listTail :: List a} deriving (Show, Eq, Ord, Read)
{-
        ghci> Empty  
        Empty  
        ghci> 5 `Cons` Empty  
        Cons 5 Empty  
        ghci> 4 `Cons` (5 `Cons` Empty)  
        Cons 4 (Cons 5 Empty)  
        ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))  
        Cons 3 (Cons 4 (Cons 5 Empty))
        ghci> Empty `Cons` Empty
        Cons Empty Empty

        > Cons 1 Empty
        Cons 1 Empty
        > Cons 2 it
        Cons 2 (Cons 1 Empty)
        > Cons 3 it
        Cons 3 (Cons 2 (Cons 1 Empty))
-}

listMe :: [a] -> List a
listMe [] = Empty
listMe (x:xs) = Cons x (listMe xs)
-- listMe "hello" returns Cons 'h' (Cons 'e' (Cons 'l' (Cons 'l' (Cons 'o' Empty))))
-- listMe [Just True, Nothing, Just False] returns Cons (Just True) (Cons Nothing (Cons (Just False) Empty))

infixr 5 :-:
data List1 a = Emptyy | a :-: (List1 a) deriving(Show, Read, Eq, Ord)

{-  A fixity states how tightly the operator binds and whether it's left-associative or right-associative. 
For instance, *'s fixity is infixl 7 * and +'s fixity is infixl 6. That means that they're both left-associative 
(4 * 3 * 2 is (4 * 3) * 2) but * binds tighter than +, because it has a greater fixity, so 5 * 4 + 3 is 
(5 * 4) + 3. Otherwise, we just wrote a :-: (List a) instead of Cons a (List a). -}

{-
        ghci> 1 :-: Emptyy
        1 :-: Emptyy
        ghci> 2 :-: it
        2 :-: (1 :-: Emptyy)
-}

infixr 5 .++
(.++) :: List1 a -> List1 a -> List1 a
Emptyy .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

{-
        ghci> 1:-: Emptyy
        1 :-: Emptyy
        ghci> 2 :-: 3 :-: Emptyy .++ it
        2 :-: (3 :-: (1 :-: Emptyy))
-}


-- Tree representation: A binary tree is either a node with two children—which are themselves binary trees, or an empty value.
data Tree' a = Node a (Tree' a) (Tree' a) | EmptyTree deriving (Show)

singleton' :: a -> Tree' a
singleton' x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree' a -> Tree' a
treeInsert x EmptyTree = singleton' x
treeInsert x (Node y left right) | x == y = Node x left right
                                 | x > y = Node y left (treeInsert x right)
                                 | x < y = Node y (treeInsert x left) right


treeElem :: Ord a => a -> Tree' a -> Bool
treeElem x EmptyTree = Prelude.False
treeElem x (Node y left right)
    | x == y = Prelude.True
    | x > y = treeElem x right
    | x < y = treeElem x left

{- 
    Inserted nodes:

        ghci> treeInsert 5 EmptyTree
        Node 5 EmptyTree EmptyTree
        ghci> treeInsert 6 it
        Node 5 EmptyTree (Node 6 EmptyTree EmptyTree)
        ghci> treeInsert 4 it
        Node 5 (Node 4 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)
-}

nums = [3,1,2,5,4]

numsTree :: Tree' Integer
numsTree = foldr treeInsert EmptyTree nums
-- Node 4 (Node 2 (Node 1 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)) (Node 5 EmptyTree EmptyTree)


data TrafficLight = Green | Yellow | Red

instance Eq TrafficLight where
    Red == Red = Prelude.True
    Green == Green = Prelude.True
    Yellow == Yellow = Prelude.True
    _ == _ = Prelude.False

instance Show TrafficLight where
    show Green = "Green Light"
    show Yellow = "Yellow Light"
    show Red = "Red Light"
    
{-
        ghci> Red == Red  
        True  
        ghci> Red == Yellow  
        False  
        ghci> Red `elem` [Red, Yellow, Green]  
        True  
        ghci> [Red, Yellow, Green]  
        [Red light,Yellow light,Green light] 
-}

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = Prelude.False
    yesno _ = Prelude.True

instance YesNo [a] where
    yesno [] = Prelude.False
    yesno _ = Prelude.True

instance YesNo Bool where
    yesno = id -- id is just a standard library function that takes a parameter and returns the same thing, which is what we would be writing here anyway.

instance YesNo (Maybe a) where
    yesno (Maybe.Just _) = Prelude.True
    yesno Maybe.Nothing = Prelude.False

instance YesNo (Tree' a) where
    yesno EmptyTree = Prelude.False
    yesno _ = Prelude.True

instance YesNo TrafficLight where
    yesno Red = Prelude.False
    yesno _ = Prelude.True

{-
        ghci> yesno $ length []  
        False  
        ghci> yesno "haha"  
        True  
        ghci> yesno ""  
        False  
        ghci> yesno $ Maybe.Just 0  
        True  
        ghci> yesno True  
        True  
        ghci> yesno EmptyTree  
        False  
        ghci> yesno []  
        False  
        ghci> yesno [0,0,0]  
        True  
        ghci> :t yesno  
        yesno :: (YesNo a) => a -> Bool
-}

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

{-
        ghci> yesnoIf [] "YEAH!" "NO!"  
        "NO!"  
        ghci> yesnoIf [2,3,4] "YEAH!" "NO!"  
        "YEAH!"  
        ghci> yesnoIf True "YEAH!" "NO!"  
        "YEAH!"  
        ghci> yesnoIf (Just 500) "YEAH!" "NO!"  
        "YEAH!"  
        ghci> yesnoIf Nothing "YEAH!" "NO!"  
        "NO!" 
-}


{-
#############################################################################################################

-- The `Num` typeclass implementation is as follows:
class Num' a where
    (+) :: a -> a-> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    abs :: a -> a
    negate :: a -> a
    fromInteger :: Integer -> a
    signum :: a -> a

-- `Eq` typeclass implementation:
class Eq'' a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- `Ord` Typeclass implementation:
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    min :: a -> a -> a
    max :: a -> a -> a

-- `Show` typeclass implementation:
class Show' a where
    showsPrec :: Int -> a ->ShowS
    show :: a -> String
    showList :: [a] -> ShowS

-- using instance
data Temperature = C Float | F Float

-- instance Eq Temperature where
--     (C n) == (C m) = n == m
--     (F n) == (F m) = n == m
--     (C c) == (F f) = (1.8 * c + 32) == f
--     (F f) == (C c) = (1.8 * c + 32) == f
--     _ == _ = False
--     f1 /= f2 = not (f1 == f2) 


-- Typeinference

-- add :: Num d => d -> d -> [d] -> [d]
--         add x y z = (x + y) : z
--     (+) :: Num d => d -> d -> d
--     (:) :: e -> [e] -> [e]

-- x :: d  y :: d  z :: [e]    z :: [d]


-- flex :: (Int a, Bool a) => a -> a -> a
-- flex a1 a2 = case (typeOf a1) of
--                 Int -> a1 + a2
--                 Bool -> a1 && a2
--                 _ -> a1
-}



-- The Functor Typeclass

instance Functor Tree' where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
    
instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f (Just x) = Just x
    fmap f Nothing = Nothing

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

instance Functor (Map.Map k) where
    fmap f (Map.Map k v) = Map.Map k (f v)

________________________________________
                                        |   
    *Main> fmap (+1) [1..3]             |
    [2,3,4]                             |
    *Main> map (+1) [1..3]              |
    [2,3,4]                             |_______________________________________________________________________
                                                                                                                |
    ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")                                  |
    Just "Something serious. HEY GUYS IM INSIDE THE JUST"                                                       |
    ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing                                                      |
    Nothing                                                                                                     |
    ghci> fmap (*3) $ Just 100                                                                                  |
    Just 300                                                                                                    |
    ghci> fmap (*3) $ Nothing                                                                                   |
    Nothing                                                                                                     |
                                                                                                                |
    ghci> fmap (*2) EmptyTree                                                                                   |
    EmptyTree                                                                                                   |
    ghci> fmap (*2) $ foldr treeInsert EmptyTree [4,1,6,8,5]                                                    |
    Node 10 (Node 2 EmptyTree (Node 8 EmptyTree EmptyTree)) (Node 16 (Node 12 EmptyTree EmptyTree) EmptyTree)   |
                                                                                                                |
                                                                                                                |
________________________________________________________________________________________________________________|

    

-}

-- Kinds and higher-kind types: https://serokell.io/blog/kinds-and-hkts-in-haskell

data Frank a b = Frank {frankField :: b a} deriving (Show)

{-
    ghci> :t Frank
    Frank :: b a -> Frank a b
    ghci> :k Frank
    Frank :: * -> (* -> *) -> *
    
    ghci> :t Frank {frankField = Just "haha"}
    Frank {frankField = Maybe.Just "haha"} :: Frank [Char] Maybe
    
    ghci> :t Frank {frankField = Node 't' EmptyTree EmptyTree}
    Frank {frankField = Node 't' EmptyTree EmptyTree}
    :: Frank Char Tree'

    ghci> :t Frank {frankField = Node "tree" EmptyTree EmptyTree}
    Frank {frankField = Node "tree" EmptyTree EmptyTree}
    :: Frank [Char] Tree'

    ghci> :t Frank {frankField = "typeMe"}
    Frank {frankField = "typeMe"} :: Frank Char []
-}

class Tofu t where
    tofu :: j a -> t j a

-- instance Tofu Frank where
--     tofu x = Frank x



data Collection' f a = Collection' (f a) deriving (Show)

listify :: Collection' [] Int
listify = Collection' [1,2,3,4,5] --returns Collection' [1,2,3,4,5]

stringify :: Collection' [] String
stringify = Collection' ["aaa","bbb"] --returns Collection' ["aaa","bbb"]

maybefy :: Collection' Maybe String
maybefy = Collection' $ Maybe.Just "Sup!" --returns Collection' (Just "Sup!")

cmap :: Functor f => (a -> b) -> Collection' f a -> Collection' f b
cmap func (Collection' a) = Collection' (fmap func a) --(<> "!") ["Hello","Fellow", "Humans"]

{- 
        ghci> cmap (<> "!") $ Collection' ["Hello","Fellow", "Humans"]
        Collection' ["Hello!","Fellow!","Humans!"]

        ghci> cmap (+2) $ Collection'[1,2,3]
        Collection' [3,4,5]
-}

-- the `<$` function: (<$) just runs fmap with const function.
--      x <$ f = fmap (const x) f

{-
        ghci> "abc" <$ [1,2,3]
        ["abc","abc","abc"]
        ghci> "abc" <$ Nothing
        Nothing
        ghci> 1 <$ []
        []
        ghci> 1 <$ [0]
        [1]
        ghci> 1 <$ [1..5]
        [1,1,1,1,1]
-}

readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
        Maybe.Nothing -> Left $ "Can't parse : " ++ s
        Maybe.Just a -> Right a

foo :: String -> String -> String -> Either String Int
foo a b c = case readEither a of
        Left err -> Left err
        Right k -> case readEither b of
                Left err -> Left err
                Right l -> case readEither c of
                        Left err -> Left err
                        Right m -> Right (k + l + m)

{-      *Main> foo "1" "2" "3"
                Right 6
        *Main> foo "" "" ""
                Left "Can't parse : "
        *Main> foo "1" "2" "abc"
                Left "Can't parse : abc"
-}

bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x) f = f x

foo' :: String -> String -> String -> Either String Int
foo' a b c = readEither a `bindEither` \k ->
                readEither b `bindEither` \l ->
                        readEither c `bindEither` \m -> Right (k + l + m)

{-      *Main> foo "1" "2" "3"
                Right 6
        *Main> foo "" "" ""
                Left "Can't parse : "
        *Main> foo "1" "2" "abc"
                Left "Can't parse : abc"
-}

data Writer' a = Writer' a [String]
    deriving (Show)

number :: Int -> Writer' Int
number n = Writer' n ["number: " ++ show n]

tell :: [String] -> Writer' ()
tell = Writer' ()

bindWriter :: Writer' a -> (a -> Writer' b) -> Writer' b
bindWriter (Writer' a xs) f =
    let Writer' b ys = f a
    in
        Writer' b $ xs ++ ys

foo3 :: Writer' Int -> Writer' Int -> Writer' Int -> Writer' Int
foo3 x y z =
    x `bindWriter` \k ->
        y  `bindWriter` \l ->
            z `bindWriter` \m -> 
            let s = k + l + m
            in tell ["Sum : " ++ show s] `bindWriter` \_ ->
                Writer' s []
-- foo3 (Writer' x xs) (Writer' y ys) (Writer' z zs) = 
    -- let s = x + y + z
    --     Writer' _ us = tell ["Sum: " ++ show s]
    -- in
    --     Writer' s $ xs ++ ys ++ zs ++ us
{- 
    Without the 'tell' function and additional let components
        *Main> foo3 (number 1) (number 2) (number 3)
            Writer' 6 ["number: 1","number: 2","number: 3"]
    With the `tell` function and additonal let components
        *Main> foo3 (number 1) (number 2) (number 3)
            Writer' 6 ["number: 1","number: 2","number: 3","Sum: 6"]
    With `bindWriter`
        *Main> foo3 (number 1) (number 2) (number 3)
            Writer' 6 ["number: 1","number: 2","number: 3","Sum: 6"]
-}



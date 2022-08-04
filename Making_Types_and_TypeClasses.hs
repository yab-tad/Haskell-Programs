
-- Exporting functions and types to a module
module Shapes (
    Shape'(..),
    Point'(..),
    baseCircle,
    baseRect,
    nudge,
    surface
) where


import GhcPlugins (all2)
import Data.List


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










{- #############################################################################################################

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
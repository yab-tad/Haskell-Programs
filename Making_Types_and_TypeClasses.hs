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
class Eq' a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- `Ord` Typeclass implementation:
class Eq' a => Ord' a where
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
-- instance Eq' Temperature where
    -- (==) (C n) (C m) = n == m
    -- (==) (F n) (F m) = n == m
    -- (==) (C c) (F f) = (1.8 * c + 32) == f
    -- (==) (F f) (C c) = (1.8 * c + 32) == f

data Temperature' = C' Float | F' Float
    deriving(Show, Eq)
    {- 
        The `Eq` derivation is equivalent to:
            (==) (C' n) (C' m) = n == m
            (==) (F' n) (F' m) = n == m
            (==) _ _ = Flase
    -}

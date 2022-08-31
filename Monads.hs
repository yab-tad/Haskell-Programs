
import Control.Monad

{-

{-# LANGUAGE KindSignatures #-}
import DynFlags (mAX_Real_Double_REG)
import BinIface (BinSymbolTable)

class Applicative m => Monad (m :: * -> *) where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    fail :: String -> m a
    {-# MINIMAL (>>=) #-}

instance Monad Maybe where
    m >>= f = case m of
                Nothing -> Nothing
                Just x -> f x
    return v = Just v
-}

maybeAdd1 :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybeAdd1 mx my = 
    mx >>= (\x -> my >>= (\y -> Just $ x+y))
{-
        ghci> maybeAdd1 (Just 2) (Just 3)
        Just 5
        ghci> maybeAdd1 Nothing (Just 3)
        Nothing
-}

maybeAdd2 :: (Num a) => Maybe a -> a -> Maybe a
maybeAdd2 mx y = 
    mx >>= (\x -> Just $ x+y)
{-
        ghci> maybeAdd2 Nothing 3
        Nothing
        ghci> maybeAdd2 (Just 3) 1
        Just 4
-}

monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my = do
                x <- mx
                y <- my
                return $ x+y
    -- mx >>= (\x -> my >>= (\y -> return $ x + y))
{-
        ghci> monadd (Just 1) (Just 2)
        Just 3
        ghci> monadd (Just 1) Nothing
        Nothing
-}


data Writer' a = Writer' a [String]
    deriving (Show)

instance Functor Writer' where
    fmap = liftM

instance Applicative Writer' where
    pure = return
    (<*>) = ap

instance Monad Writer' where
    return a = Writer' a []
    (>>=) = bindWriter

number :: Int -> Writer' Int
number n = Writer' n ["number: " ++ show n]

tell :: [String] -> Writer' ()
tell = Writer' ()

bindWriter :: Writer' a -> (a -> Writer' b) -> Writer' b
bindWriter (Writer' a xs) f =
    let Writer' b ys = f a
    in
        Writer' b $ xs ++ ys

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz = 
    mx >>= \k ->
        my >>= \l ->
            mz >>= \m -> 
                let s = k + l + m in return s

threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s

foo :: Writer' Int -> Writer' Int -> Writer' Int -> Writer' Int
foo x y z =
    x `bindWriter` \k ->
        y  `bindWriter` \l ->
            z `bindWriter` \m -> 
            let s = k + l + m
            in tell ["Sum : " ++ show s] `bindWriter` \_ ->
                Writer' s []

foo' :: Writer' Int -> Writer' Int -> Writer' Int -> Writer' Int
foo' x y z = do
    s <- threeInts x y z
    tell ["sum : " ++ show s]
    return s

{-
        *Main> foo (number 1) (number 2) (number 3)
                Writer' 6 ["number: 1","number: 2","number: 3","Sum : 6"]
        *Main> foo' (number 1) (number 2) (number 3)
                Writer' 6 ["number: 1","number: 2","number: 3","sum : 6"]
-}
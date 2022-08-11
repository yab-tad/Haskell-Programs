--
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
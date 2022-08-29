--
--

greet :: IO()
greet = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ ".")

{-
        *Main> greet
        What's your name?
        Yeabsira
        Hello Yeabsira.    
-}

main :: IO()
main = do
    i <- getLine
    if i /= "quit" then do
        putStrLn ("i: " ++ i)
        main
    else
        return ()

{- 
        Hello
        i: Hello
        bye bye!
        i: bye bye!
        quit
-}

count :: Int -> Int -> IO()
count n m = do
    putStrLn(show n)
    if n < m then 
        count (n+1) m
    else
        return ()

{-
        *Main> count 2 4
        2
        3
        4
-}

main2 :: IO()
main2 = bar

bar :: IO()
bar = getLine >>= \s ->
      getLine >>= \t ->
      putStrLn (s ++ t)
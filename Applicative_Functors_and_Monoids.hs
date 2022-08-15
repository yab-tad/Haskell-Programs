--
--
rev1 :: IO()
rev1 = do
        line <- getLine
        let line' = reverse line
        putStrLn $ "Your input in its original form is " ++ line ++ "!"
        putStrLn $ "Your input reversed is " ++ line' ++ "!"

rev2 :: IO()
rev2 = do
        line <- fmap reverse getLine
        putStrLn $ "Your input reversed looks like this: " ++ line


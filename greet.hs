getName :: IO String
getName = putStrLn "Input your name" >> getLine

greet :: IO ()
greet = getName >>= putStrLn . ("Hello, " ++)

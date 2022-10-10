module Print1 where

    myGreeting :: String -> String
    myGreeting x = 
        ("hello, " ++ x)

    hello :: String
    hello = "Hello,"

    world :: String
    world = "World!"

    main :: IO()
    main = do
        putStrLn (myGreeting "world!")
        putStrLn secondGreeting
        where secondGreeting = concat [hello, " ", world]
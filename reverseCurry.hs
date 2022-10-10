module ReverseCurry where

    returnNthCharOfString :: Int -> String -> Char
    returnNthCharOfString x y = 
        y !! x

    
    reverseCurry :: String -> String
    reverseCurry x = 
        (drop 9 x) ++ [(returnNthCharOfString 5 x),(returnNthCharOfString 6 x),(returnNthCharOfString 7 x), (returnNthCharOfString 8 x)] ++ (take 5 x)

    whatIsCurry :: String
    whatIsCurry = "Curry is awesome"

    main :: IO()
    main = print $ reverseCurry whatIsCurry
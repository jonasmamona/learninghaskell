module StringManipulation where
    identityString :: String -> String
    identityString x = x

    returnFifthChar :: String -> Char
    returnFifthChar x = 
        x !! 4

    dropNine :: String -> String
    dropNine x = 
        drop 9 x

    returnThirdChar :: String -> Char
    returnThirdChar x = 
        (!!) x 2
    
    returnNthCharOfJonas :: Int -> Char
    returnNthCharOfJonas x = 
        (!!) "Jonas" x

    returnNthCharOfString :: Int -> String -> Char
    returnNthCharOfString x y = 
        y !! x

    whatIsCurry :: String
    whatIsCurry = "Curry is awesome"

    returnYFromCurry :: Char
    returnYFromCurry = 
        whatIsCurry !! 4

    returnAwesome :: String
    returnAwesome = 
        drop 9 whatIsCurry

    reverseCurry :: String -> String
    reverseCurry x = 
        (drop 9 x) ++ [(returnNthCharOfString 5 x),(returnNthCharOfString 6 x),(returnNthCharOfString 7 x), (returnNthCharOfString 8 x)] ++ (take 5 x)
module Testing where

vowels :: [Char]
vowels = ['a', 'e', 'i', 'o','u']

input :: [String]
input = ["baepd", "liops", "psmda"]

isVowel :: Char -> Bool
isVowel = flip elem vowels

isEmpty :: [a] -> Bool
isEmpty = null

hasSequentialVowelPair :: String -> Bool
hasSequentialVowelPair [] = False
hasSequentialVowelPair (x:xs)
    | not $ null xs = isVowel x && isVowel (head xs) || hasSequentialVowelPair xs
    | otherwise = False

getIndexOfSequentialVowelPair :: String -> Int
getIndexOfSequentialVowelPair list = go list 0
    where
        go :: String -> Int -> Int
        go [] result = result
        go (x:xs) result
            | null xs = result
            | isVowel x && isVowel (head xs) = result
            | otherwise = go xs (result + 1)
        
getPositionOfSequentialVowelPair :: String -> Maybe Int
getPositionOfSequentialVowelPair [] = Nothing
getPositionOfSequentialVowelPair xs = if hasSequentialVowelPair xs then Just $ getIndexOfSequentialVowelPair xs else Nothing

maybeGetIndexOfPairPerRow :: [String] -> Maybe [Int]
maybeGetIndexOfPairPerRow [] = Nothing
maybeGetIndexOfPairPerRow matrix = sequence $ go matrix []
    where
        go :: [String] -> [Maybe Int] -> [Maybe Int]
        go [] results = results
        go (x:xs) results
            | hasSequentialVowelPair x = go xs (results ++ [Just $ getIndexOfSequentialVowelPair x])
            | otherwise = go xs results

doesMatrixHave2x2VowelSquare :: [String] -> String
doesMatrixHave2x2VowelSquare matrix = 
    case maybeGetIndexOfPairPerRow matrix of
        Nothing -> "No"
        Just xs -> go xs
            where
                go :: [Int] -> String
                go [] = "No"
                go (x:xs)
                    | null xs = "No"
                    | x == head xs = "Yes"
                    | otherwise = go xs
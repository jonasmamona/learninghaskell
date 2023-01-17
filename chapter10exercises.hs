module Chapter10Exercises where

    stops = "pbtdkg"
    vowels = "aeiou"

    permutateOverVowels firstLetter lastLetter = 
        map (\x -> (firstLetter, x, lastLetter)) vowels

    permutateStops (x:xs) = 
        case xs of
            [] -> []
            _ -> map (\y -> (x, y, head xs)) vowels

    fst' (a,b,c) = a
    
    permutateStopsOverVowels list@(x:xs) acc = 
        case xs of
            [] -> acc
            _ -> permutateStopsOverVowels xs (permutateStops list : acc)

    permutateStopsOverVowelsOnlyP list@(x:xs) acc = 
        case xs of
            [] -> foldr (\current accumulator -> if fst' (head current) == 'p' then current : accumulator else accumulator) [] acc
            _ -> permutateStopsOverVowels xs (permutateStops list : acc)
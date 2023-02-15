module Chapter11Exercises where
import Data.Char
import Cipher (shiftCharsRight)

testPhrase = "MEET ME AT DAWN"
testKeyword :: String
testKeyword = "ALLY"

substituteChar :: Int -> String -> (Char, Int)
substituteChar index keyword = (keyword !! calculated, calculated + 1)
    where calculated = if index + 1 > length keyword then 0 else index

substituteRecursively :: String -> String -> String
substituteRecursively input keyword = internalFunction input [] 0
    where 
        internalFunction :: String -> String -> Int -> String
        internalFunction [] result _ = result
        internalFunction (x:xs) result acc 
            | x == ' '  = internalFunction xs (result ++ [x]) acc
            | otherwise = internalFunction xs (result ++ [fst currentSubstitution]) (snd currentSubstitution)
                where currentSubstitution = substituteChar acc keyword

convertSubstitutionToCypherShiftValues :: String -> [Int]
convertSubstitutionToCypherShiftValues = map (\x -> ord x - ord 'A') 

getShiftValues :: String -> String -> [Int]
getShiftValues input keyword = convertSubstitutionToCypherShiftValues $ substituteRecursively input keyword

checkAnswer = substituteRecursively testPhrase testKeyword == "ALLY AL LY ALLY"

getValidationShiftValueList = convertSubstitutionToCypherShiftValues $ substituteRecursively testPhrase testKeyword

vigenereCipher :: String -> String -> String
vigenereCipher input keyword = zipWith (flip shiftCharsRight) input $ getShiftValues input keyword

-- isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
-- isSubseqOf [] _ = True
-- isSubseqOf a@(x:xs) b@(y:ys) = 
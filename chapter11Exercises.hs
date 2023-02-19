module Chapter11Exercises where
import Data.List ( maximumBy, elemIndex, group, sort, sortOn )
import Data.Function ( on )    
import Data.Char ( ord, isUpper, toLower, isSpace )
import Cipher (shiftCharsRight)
import qualified Data.Char as Char
import Lists (mySplit)
import Data.Maybe

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

isSubseqOfNonSequential :: (Eq a) => [a] -> [a] -> Bool
isSubseqOfNonSequential [] _ = True
isSubseqOfNonSequential a@(x:xs) b@(y:ys) = x == y && isSubseqOfNonSequential xs ys

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf maybeSubseq originalSeq = internalFunction maybeSubseq originalSeq []
    where 
        internalFunction ::(Eq a) => [a] -> [a] -> [a] -> Bool
        internalFunction [] _ acc = length acc == length maybeSubseq
        internalFunction _ [] acc = False
        internalFunction target@(x:xs) (y:ys) acc
            | x == y = internalFunction xs ys (acc ++ [x])
            | otherwise = internalFunction target ys acc

betterIsSubseqOf :: (Eq a) => [a] -> [a] -> Bool
betterIsSubseqOf [] _ = True
betterIsSubseqOf _ [] = False
betterIsSubseqOf a@(x:xs) b@(y:ys) = (x == y && betterIsSubseqOf xs ys) || betterIsSubseqOf a ys

capitalizeWord :: String -> String
capitalizeWord (x:xs) = Char.toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords phrase = foldr (\word@(x:xs) acc -> (word, capitalizeWord word) : acc ) [] $ words phrase 

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph phrase = internalFunction phrase [] True
    where 
        internalFunction :: String -> String -> Bool -> String
        internalFunction [] acc capitalizeNext = acc
        internalFunction string@(x:xs) acc capitalizeNext
            | x == '.' = internalFunction xs (acc ++ [x]) True
            | otherwise = if capitalizeNext then internalFunction xs (acc ++ [Char.toUpper x]) False else internalFunction xs (acc ++ [x]) False

data Key = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Zero | Star | Pound deriving (Eq, Show)

data Phone = Phone [(Key, [Char])] deriving (Eq, Show)

type Presses = Int

defaultPhone  = Phone [(One, "1"), (Two, "abc2"), (Three, "def3"), (Four, "ghi4"), (Five, "jkl5"), (Six, "mno6"), (Seven, "pqrs7"), (Eight, "tuv8"), (Nine, "wxyz9"), (Zero, " 0"), (Star, "^"), (Pound, ".,#")]

getKeyCharactersNoLookup :: Phone -> Key -> String
getKeyCharactersNoLookup (Phone ((x,y):xys)) key = if x == key then y else getKeyCharactersNoLookup (Phone xys) key

getKeyForCharacter :: Phone -> Char -> Key
getKeyForCharacter (Phone ((x,y):xys)) char = if char `elem` y then x else getKeyForCharacter (Phone xys) char

getKeyCharacters :: Phone -> Key -> Maybe String
getKeyCharacters (Phone phone) key = lookup key phone

defaultPhoneGetCharactersForKey :: Key -> String
defaultPhoneGetCharactersForKey = getKeyCharactersNoLookup defaultPhone

defaultPhoneGetKeyForCharacter :: Char -> Key
defaultPhoneGetKeyForCharacter = getKeyForCharacter defaultPhone

getTaps :: Char -> String -> Presses
getTaps char string =  1 + fromJust (char `elemIndex` string)

getTapsForCharacter :: Char -> (Key, Presses)
getTapsForCharacter char = (defaultPhoneGetKeyForCharacter innerChar, getTaps innerChar $ defaultPhoneGetCharactersForKey $ defaultPhoneGetKeyForCharacter innerChar) where innerChar = toLower char

reverseTaps :: Char -> [(Key, Presses)]
reverseTaps char = if isUpper char then [(Star, 1), getTapsForCharacter char] else [getTapsForCharacter char]

convo :: [String]
convo = ["Wanna play 20 questions","Ya","U 1st haha","Lol ok. Have u ever tasted alcohol","Lol ya","Wow ur cool haha. Ur turn","Ok. Do u think I am pretty Lol","Lol ya","Just making sure rofl ur turn"]

message :: String
message = "My name is jonass"

convertMessageToTaps :: String -> [(Key, Presses)]
convertMessageToTaps = foldr (\x acc -> reverseTaps x ++ acc) []

getTotalPressesForMessage :: [(Key, Presses)] -> Int
getTotalPressesForMessage = foldr (\(x,y) acc -> y + acc) 0 

convertConversationToTaps :: [String] -> [[(Key, Presses)]]
convertConversationToTaps = map convertMessageToTaps

calculateCostForLetter :: Char -> Presses
calculateCostForLetter letter = foldr (\(x,y) acc -> acc + y) 0 (reverseTaps letter)

mostPopularLetter phrase = (last $ sortOn length $ group $ sort $ filter (not . isSpace) $ map toLower phrase) !! 0

mostPopularLetterAndCost :: String -> (Char, Presses)
mostPopularLetterAndCost string = (thisMostPopularLetter, calculateCostForLetter thisMostPopularLetter) where thisMostPopularLetter = mostPopularLetter string

convertConversationToLongPhrase :: Foldable t => t [Char] -> [Char]
convertConversationToLongPhrase = foldr (\x acc -> x ++ " " ++ acc) []

coolestWord conversation = (last $ group $ sort $ words $ map toLower $ convertConversationToLongPhrase conversation) !! 0


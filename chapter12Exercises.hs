module Chapter12Exercises where

import Data.Maybe

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq)

type ValidatePerson a = Either [PersonInvalid] a

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age >= 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= "" then Right name else Left [NameEmpty]

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

notThe :: String -> Maybe String
notThe input = if input == "the" then Nothing else Just input

replaceThe :: String -> String
replaceThe input = unwords $ map (fromMaybe "a" . notThe) $ words input

replaceTheRecursive :: String -> String
replaceTheRecursive input = case notThe $ head $ words input of
  Nothing -> "a " ++ replaceThe (unwords (tail (words input)))
  Just word -> word ++ replaceThe (unwords (tail (words input)))

myReplaceThe :: String -> String
myReplaceThe input =
  unwords
    $ map
      ( \x ->
          case notThe x of
            Nothing -> "a"
            Just x -> x
      )
    $ words input

myReplaceTheFoldr :: String -> String
myReplaceTheFoldr input =
  unwords
    $ foldr
      ( \x acc -> case notThe x of
          Nothing -> "a" : acc
          Just x -> x : acc
      )
      []
    $ words input

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel char = char `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input = go (words input) 0
  where
    go [] acc = acc
    go (x : xs) acc = if x == "the" && isVowel (head (head xs)) then go xs (acc + 1) else go xs acc

countVowels :: String -> Integer
countVowels input = go input 0
  where
    go :: String -> Integer -> Integer
    go [] acc = acc
    go (x : xs) acc = if isVowel x then go xs (acc + 1) else go xs acc

newtype Word' = Word' String deriving (Eq, Show)

getCountOfVowelsAndConsonants :: String -> (Integer, Integer)
getCountOfVowelsAndConsonants input = go input 0 0
  where
    go :: String -> Integer -> Integer -> (Integer, Integer)
    go [] vowels consonants = (vowels, consonants)
    go (x : xs) vowels consonants = if isVowel x then go xs (vowels + 1) consonants else go xs vowels (consonants + 1)

mkWord :: String -> Maybe Word'
mkWord input = if fst result > snd result then Just (Word' input) else Nothing
  where
    result = getCountOfVowelsAndConsonants input
module Chapter12Exercises where

import Data.List
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

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ number) = 1 + natToInteger number

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat number = Succ (integerToNat (number - 1))

isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' (Just a) = True

isNothing' :: Maybe a -> Bool
isNothing' (Just a) = False
isNothing' Nothing = True

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing = b
mayybe b f (Just a) = f a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing = mayybe a id Nothing
fromMaybe' a (Just b) = mayybe a id (Just b)

listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' (x : xs) = Just x

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just a) = [a]

catMaybes' :: [Maybe a] -> [a]
catMaybes' items = go items []
  where
    go :: [Maybe a] -> [a] -> [a]
    go [] acc = acc
    go (x : xs) acc =
      case x of
        Nothing -> go xs acc
        Just x -> go xs (acc ++ [x])

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' list = Just (catMaybes' list)

lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    ( \x acc -> case x of
        Left a -> a : acc
        Right _ -> acc
    )
    []

rights' :: [Either a b] -> [b]
rights' =
  foldr
    ( \x acc -> case x of
        Right a -> a : acc
        Left _ -> acc
    )
    []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = (lefts' list, rights' list)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f a =
  case a of
    Right a -> Just (f a)
    Left _ -> Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g a =
  case a of
    Left a -> f a
    Right a -> g a

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f a =
  case f a of
    Nothing -> []
    Just (x, y) -> x : myUnfoldr f y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f a = case f a of
  Nothing -> Leaf
  Just (x, y, z) -> Node (unfoldTree f x) y (unfoldTree f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree f 0
  where 
    f x 
      | x == n = Nothing
      | otherwise = Just (x+1, x, x+1)
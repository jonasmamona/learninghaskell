module Chapter13Exercises where

import Control.Monad
import Data.Char (isLetter, toLower)
import System.Exit (exitSuccess)

getOnlyLetters :: String -> String
getOnlyLetters = filter isLetter

stringToLower :: String -> String
stringToLower = map toLower

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case ((getOnlyLetters $ stringToLower line1) == (reverse $ getOnlyLetters $ stringToLower line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $ "Name was : " ++ show name ++ " Age was : " ++ show age

gimmeAge :: IO Age
gimmeAge = do 
    putStrLn "Gimme a persons age"
    maybeAge <- getLine
    return (read maybeAge :: Age)

gimmeName :: IO String
gimmeName = do 
    putStrLn "Gimme a persons name"
    maybeName <- getLine
    return maybeName

gimmePerson :: IO ()
gimmePerson = do
    age <- gimmeAge
    name <- gimmeName
    case mkPerson name age of
        Left error -> do
            putStrLn "You dun goofed"
            print error
        Right person -> do
            putStrLn "got a person over here"
            print person

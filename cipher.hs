module Cipher where

import Data.Char
import Text.Read (readMaybe)

shiftCharsRight :: Int -> Char -> Char
shiftCharsRight _ ' ' = ' '
shiftCharsRight n x = chr (specialOrd n x)
  where
    specialOrd n x
      | ord x + n > 122 = (ord x + n) - 26
      | otherwise = ord x + n

shiftCharsLeft :: Int -> Char -> Char
shiftCharsLeft _ ' ' = ' '
shiftCharsLeft n x = if ord x == (-33) then ' ' else chr (specialOrd n x)
  where
    specialOrd n x
      | ord x - n < 97 = (ord x - n) + 26
      | otherwise = ord x - n

caesarCipher :: [Char] -> Int -> [Char]
caesarCipher input shiftValue = map (shiftCharsRight shiftValue) input

reverseCaesarCipher :: [Char] -> Int -> [Char]
reverseCaesarCipher input shiftValue = map (shiftCharsLeft shiftValue) input

caesarCipher' :: [Char] -> Int -> IO [Char]
caesarCipher' input shiftValue = return $ caesarCipher input shiftValue

reverseCaesarCipher' :: [Char] -> IO [Char]
reverseCaesarCipher' input = return $ reverseCaesarCipher input 10

getShiftValueFromUser :: IO Int
getShiftValueFromUser = do
  putStrLn "type shift value"
  shiftValue <- getLine
  case (readMaybe shiftValue :: Maybe Int) of
    Nothing -> do
      putStrLn "Invalid input, please type an integer"
      getShiftValueFromUser
    Just a ->
      return a

main :: IO ()
main = do
  putStrLn "type a word"
  word <- getLine
  shiftValue <- getShiftValueFromUser
  putStrLn "shifted"
  shifted <- caesarCipher' word shiftValue
  putStrLn shifted
  putStrLn "shifted back"
  putStrLn (reverseCaesarCipher shifted shiftValue)
  return ()
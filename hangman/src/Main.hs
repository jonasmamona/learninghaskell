module Main (main) where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

type GuessCount = Int

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLenght :: Int
maxWordLenght = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w = let l = Prelude.length w in l >= minWordLength && l < maxWordLenght

generateRandomWord :: WordList -> IO String
generateRandomWord (WordList wl) = do
  randomIndex <- randomRIO (0, Prelude.length wl - 1)
  return $ wl !! randomIndex

randomWord :: IO String
randomWord = gameWords >>= generateRandomWord

data Puzzle = Puzzle String [Maybe Char] [Char] GuessCount

instance Show Puzzle where
  show (Puzzle _ discovered guessed _) =
    intersperse ' ' $ fmap renderPuzzleChar discovered ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle input = Puzzle input (map (const Nothing) input) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle input _ _ _) character = character `elem` input

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) character = character `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just character) = character

fillInCharacter :: Puzzle -> Char -> GuessCount -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s _) c newGuesses =
  Puzzle word newFilledInSoFar (c : s) newGuesses
  where
    zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle _ _ _ guesses) guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in!"
      return (fillInCharacter puzzle guess guesses)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess (guesses + 1))

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ guessCount) =
  if guessCount > 10
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle 
  gameWin puzzle
  putStrLn $ "Current puzzle is " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

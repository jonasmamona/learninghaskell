module Main (main) where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import System.Environment (getArgs)
import System.Exit (exitFailure,exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)
import Data.ByteString (hGetLine)
import Data.ByteString.Char8 (putStrLn)
import System.Directory.Internal.Prelude (exitFailure)
import Morse

convertToMorse :: IO ()
convertToMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess
    line <- System.IO.hGetLine stdin
    convertLine line
    where 
        convertLine line = do
            let morse = stringToMorse line
            case morse of 
                (Just str) -> Prelude.putStrLn (intercalate " " str)
                Nothing -> do
                    Prelude.putStrLn $ "ERROR: " ++ line
                    exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess
    line <- System.IO.hGetLine stdin
    convertLine line
    where 
        convertLine line = do
            let decoded = traverse morseToChar (words line)
            case decoded of
                (Just s) -> Prelude.putStrLn s
                Nothing -> do
                    Prelude.putStrLn $ "ERROR: " ++ line
                    exitFailure

main :: IO ()
main = do
    mode <- getArgs
    case mode of
        [arg] -> 
            case arg of
                "from" -> convertFromMorse
                "to" -> convertToMorse
                _ -> argError
        _ -> argError
        where argError = do exitFailure
module Cipher where

import Data.Char

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

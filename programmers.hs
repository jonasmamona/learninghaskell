module Programmers where

    data OperatingSystem = Linux | OpenBSD | Mac | Windows deriving (Eq,Show)

    data ProgLang = Haskell | C | CSharp | Typescript deriving (Eq,Show)
 
    data Programmer = Programmer {os :: OperatingSystem, lang :: ProgLang} deriving (Eq,Show)

    allOperatingSystems :: [OperatingSystem]
    allOperatingSystems = [Linux, OpenBSD, Mac, Windows]

    allLanguages :: [ProgLang]
    allLanguages = [Haskell, C, CSharp, Typescript]

    allCombinations :: [Programmer]
    allCombinations = foldr (\x acc -> (foldr (\y acc2 -> (Programmer {os = y, lang = x}) : acc2) [] allOperatingSystems) ++ acc) []  allLanguages


    
module AlgebraicDataTypes where

    data Doggies a = 
        Husky a | Mastiff a deriving (Eq, Show)

    stringDoggies :: Doggies String
    stringDoggies = Husky "a"

    data DogueDeBordeaux doge = DogueDeBordeaux doge
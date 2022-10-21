{-# LANGUAGE InstanceSigs #-}
module TypleclassInstantiation where 
    newtype TisAnInteger = TisAn Int deriving Show

    instance Eq TisAnInteger where
        (==) (TisAn x) (TisAn y) = x == y

    data TwoIntegers = Two Int Int

    instance Eq TwoIntegers where
        (==) (Two x y) (Two w z) = x == w && y == z

    data StringOrInt = 
        TisAnInt Int | TisAString [Char] deriving Show

    instance Eq StringOrInt where
        (==) (TisAnInt x) (TisAnInt y) = x == y
        (==) (TisAString x) (TisAString y) = x == y
        (==) _ _ = False

    data Pair a = Pair a a deriving Show

    instance Eq a => Eq (Pair a) where 
        (==) (Pair x y) (Pair w z) = x == y

    data Tuple a b = Tuple a b
    instance (Eq a, Eq b) => Eq (Tuple a b) where
        (==) (Tuple x y) (Tuple w z) = x == w && y == z

    data Which a = 
        ThisOne a | ThatOne a 

    instance Eq a => Eq (Which a) where 
        (==) (ThisOne a) (ThisOne b) = a == b
        (==) (ThatOne a) (ThatOne b) = a == b
        (==) (ThisOne a) (ThatOne b) = a == b
        (==) _ _ = False

    data EitherOr a b = 
        Hello a | GoodBye b

    instance (Eq a, Eq b) => Eq (EitherOr a b) where
        (==) (Hello a) (Hello b) = a == b
        (==) (GoodBye a) (GoodBye b) = a == b
        (==) (GoodBye _) (Hello _) = False
        (==) (Hello _) (GoodBye _) = False
        
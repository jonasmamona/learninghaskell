{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module AlgebraicDataTypes where

    data Doggies a = 
        Husky a | Mastiff a deriving (Eq, Show)

    stringDoggies :: Doggies String
    stringDoggies = Husky "a"

    data DogueDeBordeaux doge = DogueDeBordeaux doge

    data Price = Price Integer deriving (Eq, Show)

    data Size = Size Integer deriving (Eq, Show)

    data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

    data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq,Show)

    data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)


    myCar = Car Mini (Price 14000)
    urCar = Car Mazda (Price 20000)
    clownCar = Car Tata (Price 7000)
    doge = Plane PapuAir (Size 1)

    cars = [myCar, urCar, doge]

    isCar :: Vehicle -> Bool
    isCar (Car _ _)= True
    isCar _ = False

    isPlane :: Vehicle -> Bool
    isPlane (Plane _ (Size _))= True
    isPlane _ = False

    areCars :: [Vehicle] -> Bool
    areCars [] = True
    areCars xs = foldr ((&&) . isCar) True xs

    getManufacturer :: Vehicle -> Maybe Manufacturer
    getManufacturer (Car x b) = Just x
    getManufacturer _ = Nothing

    data Example = MakeExample deriving Show
    data Example2 = MakeExample2 Int deriving Show

    newtype Cows = Cows Int deriving (Eq,Show)
    newtype Goats = Goats Int deriving (Eq, Show, TooMany)

    class TooMany a where
        tooMany :: a -> Bool

    instance TooMany Int where
        tooMany n = n > 42

    instance TooMany (Int, String) where
        tooMany (x,y) = x > 42 && y == "Baaaaaa"

    instance (Num a, TooMany a, Ord a) => TooMany (a,a) where
        tooMany :: (Num a, TooMany a) => (a, a) -> Bool
        tooMany (x,y) =  (x + y) > 88

    data LogicGoat = LogicGoat (Int, String) deriving (Eq, Show)

    instance TooMany LogicGoat where
        tooMany (LogicGoat(x,y)) = x > 42 && y == "Baaaaaa"

    newtype GoatOfManyLogics = GoatOfManyLogics (Int,Int) deriving (Eq, Show, TooMany)

    data BigSmall =
        Big Bool
        | Small Bool
        deriving (Eq, Show)
    
    data Person = Person {personName :: String, age :: Int} deriving (Eq, Show)

    me = Person "Jonas" 27
    myName = personName me

    -- data Fiction = Fiction deriving Show
    -- data NonFiction = NonFiction deriving Show
    -- data BookType = FictionBook Fiction | NonFictionBook NonFiction deriving Show

    type AuthorName = String
    data Author = Fiction AuthorName | NonFiction AuthorName


    data Expr = Number Int | Add Expr Expr | Minus Expr | Multi Expr Expr | Divide Expr Expr

    myAdd = Add (Number 1) (Number 2)
    myMinus = Minus (Add (Number 1) (Number 2))

    type Gardener = String
    data Garden = Gardenia Gardener | Rose Gardener | Daisy Gardener | Lilac Gardener

    data GuessWhat = ChickenButt deriving (Eq, Show)

    data Id a = MkId a deriving (Eq, Show)

    data Product a b = Product a b deriving (Eq, Show)

    data Sum a b = First a | Second b deriving (Eq, Show)

    data RecordProduct a b = RecordProduct {pfirst :: a , psecond :: b} deriving (Eq, Show)

    newtype NumCow = NumCow Int deriving (Eq, Show)
    newtype NumPig = NumPig Int deriving (Eq, Show)

    data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

    type Farmhouse' = Product NumCow NumPig

    type Name = String
    type Age = Int 
    type LovesMud = Bool
    type PoundsOfWool = Int

    data CowInfo = CowInfo Name Age deriving (Eq, Show)
    data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
    data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

    data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)

    type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

    bess' = (CowInfo "bess" 4)
    bess = First bess' :: Animal'

    e' = Second (SheepInfo "Elmer" 5 5)
    a' = First (SheepInfo "Elmer" 5 5)
    c' = First (PigInfo "Elmer" 5 True)

    elmo' = Second (SheepInfo "elmo" 5 5)

    data Twitter = Twitter deriving (Eq, Show)
    data AskFm = AskFm deriving (Eq, Show)

    newtype Name2 = Name2 String deriving Show
    newtype Acres = Acres Int deriving Show

    data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

    data Farmer = Farmer Name2 Acres FarmerType deriving Show

    isDairyFarmer :: Farmer -> Bool
    isDairyFarmer (Farmer _ _ DairyFarmer) = True
    isDairyFarmer _ = False

    data FarmerRec = FarmerRec {name :: Name2, acres :: Acres, farmerType :: FarmerType} deriving Show

    isDairyFarmerRec :: FarmerRec -> Bool
    isDairyFarmerRec farmer = 
        case farmerType farmer of
            DairyFarmer -> True
            _ -> False

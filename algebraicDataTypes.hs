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
module AlgebraicDataTypes where

    data Doggies a = 
        Husky a | Mastiff a deriving (Eq, Show)

    stringDoggies :: Doggies String
    stringDoggies = Husky "a"

    data DogueDeBordeaux doge = DogueDeBordeaux doge

    data Price = Price Integer deriving (Eq, Show)

    data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

    data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq,Show)

    data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

    myCar = Car Mini (Price 14000)
    urCar = Car Mazda (Price 20000)
    clownCar = Car Tata (Price 7000)
    doge = Plane PapuAir

    cars = [myCar,urCar,doge]

    isCar :: Vehicle -> Bool
    isCar (Car _ _)= True
    isCar _ = False

    isPlane :: Vehicle -> Bool
    isPlane (Plane _)= True
    isPlane _ = False

    areCars :: [Vehicle] -> Bool
    areCars = foldr (\x acc -> if isCar x then True else acc) False
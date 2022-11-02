module RegisteredUser where

newtype Username = Username String deriving Show

newtype AccountNumber = AccountNumber Integer deriving Show

data User = UnregisteredUser | RegisteredUser Username AccountNumber deriving Show

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "Unregistered User"
printUser (RegisteredUser (Username name) (AccountNumber accNum)) =
  putStrLn $ name ++ " " ++ show accNum

isUserRegistered :: User -> Bool
isUserRegistered (RegisteredUser (Username _) (AccountNumber _)) = True
isUserRegistered UnregisteredUser = False

data WherePenguinsLive = Galapagos | Antarctica | Australia | SouthAfrica | SouthAmerica deriving (Eq, Show)

newtype Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive 
gimmeWhereTheyLive (Peng whereItLives) = whereItLives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalagos :: Penguin -> Bool
antarcticOrGalagos p = 
    galapagosPenguin p || galapagosPenguin p
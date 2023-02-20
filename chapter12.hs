module Chapter12 where

    ifEvenAdd2 :: Integer -> Maybe Integer
    ifEvenAdd2 n = if even n then Just (n + 2) else Nothing


    type Name = String
    type Age = Integer

    data Person = Person Name Age deriving Show

    data PersonInvalid = NameEmpty | AgeTooLow deriving Eq

    type ValidatePerson a = Either [PersonInvalid] a

    toString :: PersonInvalid -> String
    toString NameEmpty = "NameEmpty"
    toString AgeTooLow = "AgeTooLow"

    instance Show PersonInvalid where
        show = toString

    ageOkay :: Age -> Either [PersonInvalid] Age
    ageOkay age = if age >= 0 then Right age else Left [AgeTooLow]

    nameOkay :: Name -> Either [PersonInvalid] Name
    nameOkay name = if name /= "" then Right name else Left [NameEmpty]

    mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
    mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
    mkPerson' (Left badName) _ = Left badName
    mkPerson' _ (Left badAge) = Left badAge

    mkPerson :: Name -> Age -> ValidatePerson Person
    mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)
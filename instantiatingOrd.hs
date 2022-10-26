module InstantiatingOrd where 

    data DayOfWeek =
        Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

    data Date = Date DayOfWeek Int deriving Show

    instance Eq DayOfWeek where 
        (==) Mon Mon = True
        (==) Tue Tue = True
        (==) Wed Wed = True
        (==) Thu Thu = True
        (==) Fri Fri = True
        (==) Sat Sat = True
        (==) Sun Sun = True
        (==) _ _  = False

    instance Eq Date where 
        (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'    

    instance Ord DayOfWeek where
        compare Fri Fri = EQ
        compare Fri _ = GT
        compare _ Fri = LT
        compare _ _ = GT
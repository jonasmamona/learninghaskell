module InstantiatingShow where 
    data Mood = Blah 

    instance Show Mood where
        show _ = "Blah"
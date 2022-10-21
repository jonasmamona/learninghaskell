module TrivialModule where 
    data Trivial = 
        Trivial'
    
    instance Eq Trivial where 
        Trivial' == Trivial' = True
    
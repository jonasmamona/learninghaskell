module TupleFunctions where
    addEmUp2 :: Num a => (a,a) -> a
    addEmUp2 (x,y) = x + y

    fst3 :: (a,b,c) -> a
    fst3 (x,_,_) = x

    k :: (a,b) -> a
    k (x,_) = x

    k1 = k (4-1, 10)

    k2 = k ("Three", 1+2)

    k3 = k (3, True)

    finalForm :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
    finalForm (a,_,c) (d,_,f) = 
        ((a,d), (c,f))
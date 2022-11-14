module Arith4 where

    roundTrip :: (Show a, Read a) => a -> a
    roundTrip a = read (show a)

    roundTripPF :: (Show a, Read a) => a -> a
    roundTripPF = read . show

    roundTrip3 :: (Show a, Read b) => a -> b
    roundTrip3 a = read (show a)

    main = do
        print (roundTrip 4)
        print (roundTripPF 4)
        print (roundTrip3 4 :: Integer)
        print (id 4)
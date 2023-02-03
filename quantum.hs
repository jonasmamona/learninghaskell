module Quantum where

    data Quantum = Yes | No | Both deriving (Eq, Show)

    quantSum1:: Either Quantum Quantum
    quantSum1 = Right Yes

    quantSum2:: Either Quantum Quantum
    quantSum2 = Right No

    quantSum3:: Either Quantum Quantum
    quantSum3 = Right Both

    quantSum4:: Either Quantum Quantum
    quantSum4 = Left Yes

    quantSum5:: Either Quantum Quantum
    quantSum5 = Left No

    quantSum6:: Either Quantum Quantum
    quantSum6 = Left Both

    convert :: Quantum -> Bool
    convert Yes = True
    convert No = True
    convert Both = True

    convert6 :: Quantum -> Bool
    convert6 Yes = True
    convert6 No = True
    convert6 Both = False

    convert5 :: Quantum -> Bool
    convert5 Yes = True
    convert5 No = False
    convert5 Both = False

    convert2 :: Quantum -> Bool
    convert2 Yes = False
    convert2 No = True
    convert2 Both = True

    convert3 :: Quantum -> Bool
    convert3 Yes = False
    convert3 No = False
    convert3 Both = True

    convert4 :: Quantum -> Bool
    convert4 Yes = False
    convert4 No = False
    convert4 Both = False

    convert7 :: Quantum -> Bool
    convert7 Yes = False
    convert7 No = True
    convert7 Both = False

    convert8 :: Quantum -> Bool
    convert8 Yes = True
    convert8 No = False
    convert8 Both = True
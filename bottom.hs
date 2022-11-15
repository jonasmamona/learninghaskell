module Bottom where
import GHC.Maybe

f :: Bool -> Maybe Int
f False = Just 0
f True = Nothing
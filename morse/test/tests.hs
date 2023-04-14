import Control.Monad
import Data.Monoid
import Test.QuickCheck
    ( frequency, quickCheck, Arbitrary(arbitrary), CoArbitrary (coarbitrary) )
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools
    mappend = (<>)

instance EqProp Bull where
    (=-=) = eq

main :: IO ()
main = do
    quickCheck (monoidAssoc :: Bull -> Bull -> Bull -> Bool)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
    quickCheck (semigroupAssoc :: Bull -> Bull -> Bull -> Bool)
    quickBatch (monoid Twoo)
    quickBatch (semigroup Twoo)
import Data.String
import GHC.Generics
import GHC.TypeLits
import Test.QuickCheck

import Data.Migration

data family Foo (a :: Nat)

newtype MyString = MyString { unMyString :: String }
  deriving (IsString, Show, Eq)

data instance Foo 0
  = FooV0
    { _fooId :: Int
    , _fooName :: String
    }
  deriving (Generic, Show, Eq)

instance Arbitrary (Foo 0) where
  arbitrary = do
    fooId <- arbitrary
    fooName <- arbitrary
    pure $ FooV0 fooId fooName

data instance Foo 1
  = FooV1
  { _fooId        :: Int
  , _fooName      :: MyString
  , _fooHonorific :: String
  }
  deriving (Generic, Show, Eq)

instance Transform Foo 0 where
  up   v = genericUp   v (const "esquire") (const MyString)
  down v = genericDown v (const unMyString)

prop_inverse_transform :: Foo 0 -> Bool
prop_inverse_transform v = (down $ up v) == v

main :: IO ()
main = quickCheck prop_inverse_transform

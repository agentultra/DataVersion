data family Foo (a :: Nat)

newtype MyString = MyString { unMyString :: String }
  deriving (IsString, Show, Eq)

data instance Foo 0
  = FooV0
    { _fooId :: Int
    , _fooName :: String
    }
  deriving (Generic, Show, Eq)

data instance Foo 1
  = FooV1
  { _fooId        :: Int
  , _fooName      :: MyString
  , _fooHonorific :: String
  }
  deriving (Generic, Show, Eq)

main :: IO ()
main = putStrLn "Test suite not yet implemented"

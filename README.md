# DataMigration

[![Hackage](https://img.shields.io/hackage/v/DataVersion.svg?logo=haskell&label=DataVersion)](https://hackage.haskell.org/package/DataVersion)

Type safe data migrations.

All you need to do is create a type family to index your record and
provide an instance of `Transform` to migrate your data between
versions.

Migrations are type-safe and the library uses generics to remove as
much boiler-plate as possible.

## Examples

```haskell
data family Foo (version :: Nat)

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

instance Transform Foo 0 where
  up   v = genericUp   v (const "esquire") (const MyString)
  down v = genericDown v (const unMyString)


spjV0 :: Foo 0
spjV0 = FooV0 1 "Simon PJ"


spjV1 :: Foo 1
spjV1 = up spjV0

-- spjV1 = FooV1 1 (MyString "Simon PJ") "esquire"
```

## Future Considerations

In the future this library will provide a high-level DSL to enable
better ergonomics around type errors so that you can see which fields
require specification in the migration.

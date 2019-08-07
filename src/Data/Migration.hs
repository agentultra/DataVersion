{-|
Module : Data.Migration
Description : Type-safe migrations for data
Copyright : (c) Sandy Maguire, 2019
                James King, 2019
License : MIT
Maintainer : james@agentultra.com
Stability : experimental

To begin migrating your data start with a type family for your record
and index it with a natural number

@
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
@

You provide an instance of the Transform class for your type in order
to specify how to transform version /n/ to version /n + 1/ and back.

Presently only simple record types are supported. More to come in the
future.
-}
module Data.Migration where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

import Data.Migration.Internal

-- | Implement this class on your type family instance to migrate
--   values of your type to the new version and back
class Transform (f :: Nat -> Type) (n :: Nat) where
  up   :: f n       -> f (n + 1)
  down :: f (n + 1) -> f n

-- | Using this function in your 'up' transformation ensures that you
-- only have to provide functions for fields that change or are added.
genericUp
    :: forall n src diff
     . ( diff ~ FieldDiff (Sort (RepToTree (Rep (src n)))) (Sort (RepToTree (Rep (src (n + 1)))))
       , GTransform diff (src n) (src (n + 1))
       , Generic (src (n + 1))
       , GUndefinedFields (Rep (src (n + 1)))
       )
    => src n -> Function diff (src n) (src (n + 1))
genericUp = gTransform @diff @_ @(src (n + 1)) undefinedFields

-- | This is the opposite of 'genericUp'.
genericDown
    :: forall n src diff
     . ( diff ~ FieldDiff (Sort (RepToTree (Rep (src (n + 1))))) (Sort (RepToTree (Rep (src n))))
       , GTransform diff (src (n + 1)) (src n)
       , Generic (src n)
       , GUndefinedFields (Rep (src n))
       )
    => src (n + 1) -> Function diff (src (n + 1)) (src n)
genericDown = gTransform @diff @_ @(src n) undefinedFields

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.String
import GHC.Generics
import GHC.TypeLits
import Test.Hspec
import Test.Hspec.QuickCheck
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

data family Bar (version :: Nat)

data instance Bar 0
  = BazV0
  | QuxV0 Int
  deriving (Eq, Generic, Show)

data instance Bar 1
  = BazV1 Int
  | QuxV1 Int
  deriving (Eq, Generic, Show)

instance Arbitrary (Bar 0) where
  arbitrary = oneof [pure BazV0, QuxV0 <$> arbitrary]

instance Transform Bar 0 where
  up = \case
    BazV0 -> BazV1 0
    QuxV0 x -> QuxV1 x
  down = \case
    BazV1 _ -> BazV0
    QuxV1 x -> QuxV0 x

main :: IO ()
main = hspec $ do
  describe "Transform" $ do
    prop "inverse a record-type migration" $ \(v :: Foo 0) ->
      (down . up $ v) `shouldBe` v

    prop "inverse a sum-type migration" $ \(v :: Bar 0) ->
      (down . up $ v) `shouldBe` v

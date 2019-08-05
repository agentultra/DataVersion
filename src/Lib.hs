{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Lib where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

data family Foo (a :: Nat)

data instance Foo 0 = FooV0 { _fooV0Id :: Int, _fooV0Name :: String }
  deriving (Generic)

data instance Foo 1
  = FooV1
  { _fooV1Id        :: Int
  , _fooV1Name      :: String
  , _fooV1Honorific :: String
  }
  deriving (Generic)

class Transform (f :: Nat -> Type) (v :: Nat) where
  up   :: f v       -> f (v + 1)
  down :: f (v + 1) -> f v

type family RepToTree (a :: Type -> Type) :: [(Symbol, Type)] where
  RepToTree (f :*: g) = RepToTree f ++ RepToTree g
  RepToTree (M1 S ('MetaSel ('Just name) _1 _2 _3) (K1 _4 t)) = '[ '(name, t) ]
  RepToTree (M1 _1 _2 f) = RepToTree f

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

type family Sort (xs :: [(Symbol, k)]) where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

type family Insert (x :: (Symbol, k)) (xs :: [(Symbol, k)]) where
  Insert x '[] = x ': '[]
  Insert '(x, t) ('(y, t') ': ys) = Insert' (CmpSymbol x y) '(x, t) '(y, t') ys

type family Insert' (b :: Ordering) (x :: (Symbol, k)) (y :: (Symbol, k)) (ys :: [(Symbol, k)]) where
  Insert' 'LT x y ys = x ': (y ': ys)
  Insert' _ x y ys = y ': Insert x ys

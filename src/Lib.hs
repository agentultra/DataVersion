{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Lib where

import Data.Generics.Product
import Lens.Micro hiding (to)
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Data.Proxy

data family Foo (a :: Nat)

data instance Foo 0
  = FooV0
    { _fooId :: Int
    , _fooName :: String
    }
  deriving (Generic, Show, Eq)

data instance Foo 1
  = FooV1
  { _fooId        :: Int
  , _fooName      :: String
  , _fooHonorific :: String
  }
  deriving (Generic, Show, Eq)

v1 = FooV1 2 "james" "sir"
v0 = FooV0 undefined undefined

v0' = copyField @"_fooName" v1 $ copyField @"_fooId" v1 v0

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

data DiffResult
  = NoChange Symbol Type
  | Addition Symbol Type
  | Change Symbol Type Type

type family Sort (xs :: [(Symbol, k)]) where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

type family Insert (x :: (Symbol, k)) (xs :: [(Symbol, k)]) where
  Insert x '[] = x ': '[]
  Insert '(x, t) ('(y, t') ': ys) = Insert' (CmpSymbol x y) '(x, t) '(y, t') ys

type family Insert' (b :: Ordering) (x :: (Symbol, k)) (y :: (Symbol, k)) (ys :: [(Symbol, k)]) where
  Insert' 'LT x y ys = x ': (y ': ys)
  Insert' _ x y ys = y ': Insert x ys

type family FieldDiff (a :: [(Symbol, Type)])
                      (b :: [(Symbol, Type)]) :: [DiffResult] where
  FieldDiff xs '[] = '[]
  FieldDiff '[] ('(y, v) ': ys) = 'Addition y v ': FieldDiff '[] ys

  FieldDiff ('(x, t) ': xs) ('(x, t) ': ys) = 'NoChange x t ': FieldDiff xs ys
  FieldDiff ('(x, u) ': xs) ('(x, v) ': ys) = 'Change x u v ': FieldDiff xs ys
  FieldDiff ('(x, u) ': xs) ('(y, v) ': ys) = FieldDiffImpl (CmpSymbol x y) '(x, u) '(y, v) xs ys

type family FieldDiffImpl (b :: Ordering)
                          (x :: (Symbol, Type))
                          (y :: (Symbol, Type))
                          (xs :: [(Symbol, Type)])
                          (ys :: [(Symbol, Type)]) :: [DiffResult] where
  FieldDiffImpl 'LT _ y xs ys = FieldDiff xs (y ': ys)
  FieldDiffImpl 'GT x '(y, v) xs ys = 'Addition y v ': FieldDiff (x ': xs) ys

copyField
    :: forall name t from to
     . ( HasField' name to   t
       , HasField' name from t
       )
    => from
    -> to
    -> to
copyField f t =
  t & field' @name .~ f ^. field' @name


class CopyAllFields (ts :: [(Symbol, Type)]) (from :: Type) (to :: Type) where
  copyAllFields :: from -> to -> to

instance CopyAllFields '[] from to where
  copyAllFields _ = id

instance ( CopyAllFields ts from to
         , HasField' name to   t
         , HasField' name from t
         ) => CopyAllFields ('(name, t) ': ts) from to where
  copyAllFields f t = copyField @name @t f $ copyAllFields @ts f t


class GUndefinedFields (o :: * -> *) where
  gUndefinedFields :: o x

instance GUndefinedFields o => GUndefinedFields (M1 _3 _4 o) where
  gUndefinedFields = M1 $ gUndefinedFields

instance (GUndefinedFields o1, GUndefinedFields o2) => GUndefinedFields (o1 :*: o2) where
  gUndefinedFields = gUndefinedFields :*: gUndefinedFields

instance GUndefinedFields (K1 _1 t) where
  gUndefinedFields = K1 undefined

undefinedFields :: (Generic t, GUndefinedFields (Rep t)) => t
undefinedFields = to gUndefinedFields

class JonkySmalls (ts :: [DiffResult]) (src :: Type) (dst :: Type)  where
  type Function ts src dst :: Type
  jonky :: dst -> src -> Function ts src dst

instance (Generic dst, GUndefinedFields (Rep dst)) => JonkySmalls '[] src dst where
  type Function '[] src dst = dst
  jonky dst _ = dst

instance ( JonkySmalls ts src dst
         , HasField' name src t
         , HasField' name dst t
         ) => JonkySmalls ('NoChange name t ': ts) src dst where
  type Function ('NoChange name t ': ts) src dst  = Function ts src dst
  jonky dst src = jonky @ts (copyField @name src dst) src

instance ( JonkySmalls ts src dst
         , HasField' name dst t
         ) => JonkySmalls ('Addition name t ': ts) src dst where
  type Function ('Addition name t ': ts) src dst  = (src -> t) -> Function ts src dst
  jonky dst src mk_t = jonky @ts (dst & field' @name .~ mk_t src) src

instance ( JonkySmalls ts src dst
         , HasField' name src ti
         , HasField' name dst to
         ) => JonkySmalls ('Change name ti to ': ts) src dst where
  type Function ('Change name ti to ': ts) src dst  = (src -> ti -> to) -> Function ts src dst
  jonky dst src mk_to = jonky @ts (dst & field' @name .~ mk_to src (src ^. field' @name)) src


doTheJonk
    :: forall src dst diff
     . ( diff ~ FieldDiff (Sort (RepToTree (Rep src))) (Sort (RepToTree (Rep dst)))
       , JonkySmalls diff src dst
       , Generic dst
       , GUndefinedFields (Rep dst)
       )
    => src -> Function diff src dst
doTheJonk = jonky @diff @src @dst undefinedFields



test
    :: Proxy (FieldDiff (Sort '[ '("id", Int) , '("name", String) ]) (Sort '[ '("id", Int) , '("name", String) , '("honorific", String)]))
   ->  Proxy '[ 'Addition "honorific" String, 'NoChange "id" Int, 'NoChange "name" String ]
test = id

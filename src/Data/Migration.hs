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

import Data.Generics.Product
import Lens.Micro hiding (to)
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import GHC.Exts

import Data.Migration.Internal

-- | Implement this class on your type family instance to migrate
--   values of your type to the new version and back
class Transform (f :: Nat -> Type) (n :: Nat) where
  up   :: f n       -> f (n + 1)
  down :: f (n + 1) -> f n

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

data DiffResult
  = NoChange Symbol Type
  | Addition Symbol Type
  | Change Symbol Type Type

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

class GTransform (ts :: [DiffResult]) (src :: Type) (dst :: Type)  where
  type Function ts src dst :: Type
  gTransform :: dst -> src -> Function ts src dst

instance (Generic dst, GUndefinedFields (Rep dst)) => GTransform '[] src dst where
  type Function '[] src dst = dst
  gTransform dst _ = dst

instance ( GTransform ts src dst
         , HasField' name src t
         , HasField' name dst t
         ) => GTransform ('NoChange name t ': ts) src dst where
  type Function ('NoChange name t ': ts) src dst  = Function ts src dst
  gTransform dst src = gTransform @ts (copyField @name src dst) src

instance ( GTransform ts src dst
         , HasField' name dst t
         ) => GTransform ('Addition name t ': ts) src dst where
  type Function ('Addition name t ': ts) src dst  = (src -> t) -> Function ts src dst
  gTransform dst src mk_t = gTransform @ts (dst & field' @name .~ mk_t src) src

instance ( GTransform ts src dst
         , HasField' name src ti
         , HasField' name dst to
         ) => GTransform ('Change name ti to ': ts) src dst where
  type Function ('Change name ti to ': ts) src dst  = (src -> ti -> to) -> Function ts src dst
  gTransform dst src mk_to = gTransform @ts (dst & field' @name .~ mk_to src (src ^. field' @name)) src

genericUp
    :: forall n src diff
     . ( diff ~ FieldDiff (Sort (RepToTree (Rep (src n)))) (Sort (RepToTree (Rep (src (n + 1)))))
       , GTransform diff (src n) (src (n + 1))
       , Generic (src (n + 1))
       , GUndefinedFields (Rep (src (n + 1)))
       )
    => src n -> Function diff (src n) (src (n + 1))
genericUp = gTransform @diff @_ @(src (n + 1)) undefinedFields

genericDown
    :: forall n src diff
     . ( diff ~ FieldDiff (Sort (RepToTree (Rep (src (n + 1))))) (Sort (RepToTree (Rep (src n))))
       , GTransform diff (src (n + 1)) (src n)
       , Generic (src n)
       , GUndefinedFields (Rep (src n))
       )
    => src (n + 1) -> Function diff (src (n + 1)) (src n)
genericDown = gTransform @diff @_ @(src n) undefinedFields

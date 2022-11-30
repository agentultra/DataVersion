module Data.Migration.Internal where

import Data.Generics.Product
import Data.Kind
import Lens.Micro hiding (to)
import GHC.Generics
import GHC.TypeLits

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

class GUndefinedFields (o :: Type -> Type) where
  gUndefinedFields :: o x

instance GUndefinedFields o => GUndefinedFields (M1 _3 _4 o) where
  gUndefinedFields = M1 $ gUndefinedFields

instance (GUndefinedFields o1, GUndefinedFields o2) => GUndefinedFields (o1 :*: o2) where
  gUndefinedFields = gUndefinedFields :*: gUndefinedFields

instance GUndefinedFields (K1 _1 t) where
  gUndefinedFields = K1 undefined

-- | Don't use this, it's not meant to be useful
undefinedFields :: (Generic t, GUndefinedFields (Rep t)) => t
undefinedFields = to gUndefinedFields

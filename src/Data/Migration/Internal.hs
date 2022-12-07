module Data.Migration.Internal where

import Data.Generics.Product
--import Data.Generics.Sum
import Data.Kind
import Lens.Micro hiding (to)
import GHC.Generics
import GHC.TypeLits

data SchemaType
  = RecordField Symbol Type
  | Ctor Symbol [Type]

-- $> :kind! Rep (Maybe Int)

type family RepToTree (a :: Type -> Type) :: [SchemaType] where
  RepToTree (f :*: g) = RepRecordFields f ++ RepRecordFields g
  RepToTree (f :+: g) = RepCtors f ++ RepCtors g
  RepToTree (M1 _1 _2 f) = RepToTree f

type family RepRecordFields (a :: Type -> Type) :: [SchemaType] where
  RepRecordFields (f :*: g) = RepRecordFields f ++ RepRecordFields g
  RepRecordFields (M1 S ('MetaSel ('Just name) _1 _2 _3) (K1 _4 t)) = '[ ('RecordField name t) ]
  RepRecordFields (M1 _1 _2 f) = RepToTree f

type family RepCtors (a :: Type -> Type) :: [SchemaType] where
  RepCtors (f :+: g) = RepCtors f ++ RepCtors g
  RepCtors (M1 C ('MetaCons name _1 _2) f) = '[ ('Ctor name (RepCtorArgs f)) ]

type family RepCtorArgs (a :: Type -> Type) :: [Type] where
  RepCtorArgs U1 = '[]
  RepCtorArgs (f :*: g) = RepCtorArgs f ++ RepCtorArgs g
  RepCtorArgs (M1 S ('MetaSel _1 _2 _3 _4) (K1 _5 t)) = '[ t ]

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  (++) '[] ys = ys
  (++) (x ': xs) ys = x ': (xs ++ ys)

type family Sort (xs :: [SchemaType]) where
  Sort '[] = '[]
  Sort (x : xs) = Insert x (Sort xs)

type family Insert (x :: SchemaType) (xs :: [SchemaType]) where
  Insert x '[] = x ': '[]
  Insert ('RecordField x t) (('RecordField y t') ': ys) = Insert' (CmpSymbol x y) ('RecordField x t) ('RecordField y t') ys
  Insert ('Ctor x ts) (('Ctor y ts' ': ys)) = Insert' (CmpSymbol x y) ('Ctor x ts) ('Ctor y ts') ys

type family Insert' (b :: Ordering) (x :: SchemaType) (y :: SchemaType) (ys :: [SchemaType]) where
  Insert' 'LT x y ys = x ': (y ': ys)
  Insert' _ x y ys = y ': Insert x ys

data DiffResult
  = NoChange Symbol Type
  | Addition Symbol Type
  | Change Symbol Type Type
  | CtorAdded Symbol [Type]
  | CtorChanged Symbol [Type] [Type]
  | CtorNoChange Symbol [Type]

type family FieldDiff (a :: [SchemaType])
                      (b :: [SchemaType]) :: [DiffResult] where
  FieldDiff xs '[] = '[]
  FieldDiff '[] (('RecordField y v) ': ys) = 'Addition y v ': FieldDiff '[] ys
  FieldDiff '[] (('Ctor y vs) ': ys) = 'CtorAdded y vs ': FieldDiff '[] ys

  FieldDiff (('RecordField x t) ': xs) (('RecordField x t) ': ys) = 'NoChange x t ': FieldDiff xs ys
  FieldDiff (('RecordField x u) ': xs) (('RecordField x v) ': ys) = 'Change x u v ': FieldDiff xs ys
  FieldDiff (('RecordField x u) ': xs) (('RecordField y v) ': ys) = FieldDiffImpl (CmpSymbol x y) ('RecordField x u) ('RecordField y v) xs ys

  FieldDiff (('Ctor x ts) ': xs) (('Ctor x ts) ': ys) = 'CtorNoChange x ts ': FieldDiff xs ys
  FieldDiff (('Ctor x us) ': xs) (('Ctor x vs) ': ys) = 'CtorChanged x us vs ': FieldDiff xs ys
  FieldDiff (('Ctor x us) ': xs) (('Ctor y vs) ': ys) = FieldDiffImpl (CmpSymbol x y) ('Ctor x us) ('Ctor y vs) xs ys

type family FieldDiffImpl (b :: Ordering)
                          (x :: SchemaType)
                          (y :: SchemaType)
                          (xs :: [SchemaType])
                          (ys :: [SchemaType]) :: [DiffResult] where
  FieldDiffImpl 'LT _ y xs ys = FieldDiff xs (y ': ys)
  FieldDiffImpl 'GT x ('RecordField y v) xs ys = 'Addition y v ': FieldDiff (x ': xs) ys

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

-- instance ( GTransform ts src dst
--          , AsConstructor' name src s
--          , AsConstructor' name dst s
--          ) => GTransform ('CtorNoChange name t ': ts) src dst where
--   type Function ('CtorNoChange name t ': ts) src dst = Function ts src dst
--   gTransform dst src = gTransform @ts _ src

class GUndefinedFields (o :: Type -> Type) where
  gUndefinedFields :: o x

instance GUndefinedFields o => GUndefinedFields (M1 _3 _4 o) where
  gUndefinedFields = M1 $ gUndefinedFields

instance (GUndefinedFields o1, GUndefinedFields o2) => GUndefinedFields (o1 :*: o2) where
  gUndefinedFields = gUndefinedFields :*: gUndefinedFields

instance GUndefinedFields (K1 _1 t) where
  gUndefinedFields = K1 undefined

-- | Don't use this, there is no honour here.
undefinedFields :: (Generic t, GUndefinedFields (Rep t)) => t
undefinedFields = to gUndefinedFields

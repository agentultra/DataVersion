module Data.Migration.Internal where

import GHC.Generics

class GUndefinedFields (o :: * -> *) where
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

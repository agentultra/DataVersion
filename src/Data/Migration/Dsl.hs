{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module Data.Migration.Dsl where

import Data.Kind
import SuperRecord

import Data.Migration.Internal

type family ToRecord (diffs :: [DiffResult]) (src :: Type) :: [Type] where
  ToRecord '[] _                       = '[]
  ToRecord ('NoChange _ _ ': xs) src   = ToRecord xs src
  ToRecord ('Addition s t ': xs) src   = (s := (src -> t)) ': ToRecord xs src
  ToRecord ('Change s t1 t2 ': xs) src = (s := (src -> t1 -> t2)) ': ToRecord xs src

foo :: Record (ToRecord '[ 'Addition ("foo") Int ] ())
foo = #foo := const 5 & rnil

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
module Dsl where

import GHC.TypeLits
import Data.Kind
import SuperRecord

import Lib

type family ToRecord (diffs :: [DiffResult]) (src :: Type) :: [Type] where
  ToRecord '[] _                      = '[]
  ToRecord (NoChange _ _ ': xs) src   = ToRecord xs src
  ToRecord (Addition s t ': xs) src   = (s := (src -> t)) ': ToRecord xs src
  ToRecord (Change s t1 t2 ': xs) src = (s := (src -> t1 -> t2)) ': ToRecord xs src

foo :: Rec (ToRecord [Addition ("foo") String, Addition ("bar") Int] ())
foo = #foo := const "yolo" & #bar := const 5 & rnil

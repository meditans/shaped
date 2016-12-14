{-# LANGUAGE DataKinds, FlexibleContexts, FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Shaped where

import Data.Text
import GHC.Generics

data Shape = Literal | Error | Validation

type family Field i a where
  Field Literal a    = a
  Field Error a      = Maybe Text
  Field Validation a = a -> Maybe Text

class Shaped a b | a -> b, b -> a where
  toShape   :: a -> b
  fromShape :: b -> a

class Shaped' (a :: *) (b :: Shape -> *) | a -> b, b -> a where
  toShape'   :: a -> b Literal
  fromShape' :: b Literal -> a

-- This should be a generic version:
-- validate :: (Shaped' a s) => a -> s Validation -> s Error
-- validate u uv = validate' (toShape u) uv
--   where
--     validateUser' :: UserShaped Literal -> UserShaped Validation -> UserShaped Error
--     validate' (s m) (s f) = UserShaped (f m)

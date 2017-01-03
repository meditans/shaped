{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, OverloadedStrings #-}

module Shaped.Tutorial where

import Data.Text
import Shaped

import Control.Monad.Identity
import Data.Functor.Classes
import Data.Functor.Compose

import           Generics.SOP hiding (Compose)
import qualified GHC.Generics as GHC

--------------------------------------------------------------------------------
-- Compact example for 3 fields, to note all the things needed
--------------------------------------------------------------------------------

-- I'd like the user of this library to be able to continue using his normal
-- datatypes, e.g.:
data User3 = User3 { userName3 :: Text, userAge3 :: Int, userDouble3 :: Double }
  deriving (Show, Read, Eq, Ord, GHC.Generic)

instance Generic User3

exUser3 = User3 "Carlo" 26 (-0.345)

exValidation :: UserShape3 (Validation (Either Text))
exValidation = UserShape3
  (Validation . Comp $ \t -> if t == "Carlo" then Right t else Left "Incorrect name")
  (Validation . Comp $ \a -> if a < 100      then Right a else Left "No cyborgs here")
  (Validation . Comp $ \d -> if d > 0        then Right d else Left "Positives values only")

-- And be able to call the generic validation function in this way:
exError :: UserShape3 (Either Text)
exError = validateRecord exUser3 exValidation

-- There is still a bit of boilerplate, to be automatically generated:

data UserShape3 f = UserShape3 (f Text) (f Int) (f Double)
  deriving (GHC.Generic)
instance Generic (UserShape3 f)

instance Shaped User3 UserShape3 where
  toShape   (User3 n a d) = UserShape3 (Identity n) (Identity a) (Identity d)
  fromShape (UserShape3 (Identity n) (Identity a) (Identity d)) = User3 n a d

instance (Show1 f) => Show (UserShape3 f) where
  showsPrec d (UserShape3 x y z) = showParen (d > 10) $
      showString "UserShape3"
    . showChar ' ' . showsPrec1 11 x
    . showChar ' ' . showsPrec1 11 y
    . showChar ' ' . showsPrec1 11 z

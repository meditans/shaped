{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Shaped.Tutorial where

import Data.Text
import Shaped

import Control.Monad.Identity
import Data.Functor.Const
import Data.Functor.Classes
import Data.Functor.Compose

import           Generics.SOP hiding (Compose)
import qualified GHC.Generics as GHC

--------------------------------------------------------------------------------
-- Compact example for 3 fields, to note all the things needed
--------------------------------------------------------------------------------

-- I'd like the user of this library to be able to continue using his normal
-- datatypes, e.g.:
data User = User { userName :: Text, userAge :: Int, userDouble :: Double }
  deriving (Show, Read, Eq, Ord, GHC.Generic)

instance Generic User

exUser = User "Carlo" 26 (-0.345)

validUser = User "Carlo" 26 0.434

exValidation :: UserShape (Validation (Either Text))
exValidation = UserShape
  (Validation . Comp $ \t -> if t == "Carlo" then Right t else Left "Incorrect name")
  (Validation . Comp $ \a -> if a < 100      then Right a else Left "No cyborgs here")
  (Validation . Comp $ \d -> if d > 0        then Right d else Left "Positives values only")

-- And be able to call the generic validation function in this way:
exError :: UserShape (Either Text)
exError = validateRecord exUser exValidation

exOk :: UserShape (Either Text)
exOk = validateRecord validUser exValidation

(tryThis, tryThat) = (transfGen exError, transfGen exOk)

-- There is still a bit of boilerplate, to be automatically generated:

data UserShape f = UserShape (f Text) (f Int) (f Double)
  deriving (GHC.Generic)
instance Generic (UserShape f)

instance Shaped User UserShape where
  toShape   (User n a d) = UserShape (Identity n) (Identity a) (Identity d)
  fromShape (UserShape (Identity n) (Identity a) (Identity d)) = User n a d

instance (Show1 f) => Show (UserShape f) where
  showsPrec d (UserShape x y z) = showParen (d > 10) $
      showString "UserShape"
    . showChar ' ' . showsPrec1 11 x
    . showChar ' ' . showsPrec1 11 y
    . showChar ' ' . showsPrec1 11 z

-----------------------------------
-- An example for the type synonyms

data BigRecord a b c = BigRecord a b c
type SmallRecord a = BigRecord () a Int

type SmallRecordShaped a f = BigRecordShaped () () Int f

-- so the advice is just to create an additional type sinonym

ex :: SmallRecordShaped Char Maybe
ex = BigRecordShaped Nothing (Just ()) (Just 3)

-- To be generated

data BigRecordShaped a b c f = BigRecordShaped (f a) (f b) (f c)
  deriving (GHC.Generic)
instance Generic (BigRecordShaped a b c f)

instance Shaped (BigRecord a b c) (BigRecordShaped a b c) where
  toShape   (BigRecord a b c) = BigRecordShaped (Identity a) (Identity b) (Identity c)
  fromShape (BigRecordShaped (Identity a) (Identity b) (Identity c)) = BigRecord a b c

instance (Show1 f, Show a, Show b, Show c) => Show (BigRecordShaped a b c f) where
  showsPrec d (BigRecordShaped x y z) = showParen (d > 10) $
      showString "UserShape"
    . showChar ' ' . showsPrec1 11 x
    . showChar ' ' . showsPrec1 11 y
    . showChar ' ' . showsPrec1 11 z

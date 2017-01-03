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

transf :: UserShape (Either Text) -> Either (UserShape (Const (Maybe Text))) User
transf us =
  let sequenced = hsequence struct
  in case sequenced of
       Right u -> Right (to u)
       Left  _ -> Left  (to . toSOPI . hmap toMaybe $ struct)
  where
    struct :: SOP (Either Text) '[ '[ Text, Int, Double ] ]
    struct = fromSOPI . from $ us

-- This has to be a natural transformation, per hmap type.
toMaybe :: Either Text t -> Const (Maybe Text) t
toMaybe x = case x of
  Right a -> Const Nothing
  Left  b -> Const (Just b)

-- Now we want to get back from a `UserShape (Either Text)` to a simpler `Either
-- (UserShape (Maybe Text)) User`, i.e. extracting the error to present the
-- eventual error in each field.

-- We have:

-- hsequence' :: (SListIN h xs, Applicative f) => h (f :.: g) xs -> f (h g xs)

-- like in:

-- hsequence' :: (SListI2 xss, Applicative f) => SOP (f :.: g) xss -> f (SOP g xss)

-- Or some of the restricted versions:

-- hsequence :: (SListIN h xs, SListIN (Prod h) xs, HSequence h) => Applicative f => h f xs -> f (h I xs)

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


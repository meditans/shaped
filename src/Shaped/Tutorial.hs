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
-- User with only one field, for the tutorial
--------------------------------------------------------------------------------

data User = User { userMail :: Text } deriving (Show, Read, Eq, Ord, GHC.Generic)
data UserShape1 f = UserShape1 { userMailF :: f Text } deriving (GHC.Generic)
instance Generic User
instance Generic (UserShape1 f)
instance (Show1 f) => Show (UserShape1 f) where
  showsPrec d (UserShape1 x) = showsUnaryWith showsPrec1 "UserShape1" d x

instance Shaped User UserShape1 where
  toShape   (User m) = UserShape1 (Identity m)
  fromShape (UserShape1 (Identity m)) = User m

exUser = User "Carlo"

exValidation1 :: UserShape1 Validation
exValidation1 = UserShape1 $ Validation $ Compose (\a -> if a == "Carlo" then Just a else Nothing)

--------------------------------------------------------------------------------
-- User with two fields, for the tutorial
--------------------------------------------------------------------------------

data User2 = User2 { userMail2 :: Text, userAge2 :: Int } deriving (Show, Read, Eq, Ord, GHC.Generic)

data UserShape2 f = UserShape2 (f Text) (f Int) deriving (GHC.Generic)

-- deriving instance (Show1 f) => Show (UserShape2 f)

instance Generic User2
instance Generic (UserShape2 f)

instance (Show1 f) => Show (UserShape2 f) where
  showsPrec d (UserShape2 x y) = showParen (d > 10) $
      showString "UserShape2"
    . showChar ' ' . showsPrec1 d x
    . showChar ' ' . showsPrec1 d y

-- TODO add a general Show instance for this kind of visualization!

showsBinaryWith' :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
    String -> Int -> a -> b -> ShowS
showsBinaryWith' sp1 sp2 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y

instance Shaped User2 UserShape2 where
  toShape   (User2 n a) = UserShape2 (Identity n) (Identity a)
  fromShape (UserShape2 (Identity n) (Identity a)) = User2 n a

exUser2 = User2 "Carlo" 45

exValidationAge :: Validation Int
exValidationAge = Validation $ Compose (\a -> if a == 26 then Just a else Nothing)

exValidation2 :: UserShape2 Validation
exValidation2 = UserShape2
  (Validation . Compose $ \t -> if t == "Carlo" then Just t else Nothing)
  (Validation . Compose $ \a -> if a < 100      then Just a else Nothing)

--------------------------------------------------------------------------------
-- Compact example for 3 fields, to note all the things needed
--------------------------------------------------------------------------------

-- I'd like the user of this library to be able to continue using his normal
-- datatypes, e.g.:
data User3 = User3 { userName3 :: Text, userAge3 :: Int, userDouble3 :: Double }
  deriving (Show, Read, Eq, Ord, GHC.Generic)

instance Generic User3

exUser3 = User3 "Carlo" 26 (-0.345)

exValidation3 :: UserShape3 Validation
exValidation3 = UserShape3
  (Validation . Compose $ \t -> if t == "Carlo" then Just t else Nothing)
  (Validation . Compose $ \a -> if a < 100      then Just a else Nothing)
  (Validation . Compose $ \d -> if d > 0        then Just d else Nothing)

exValidationGen :: UserShape3 (ValidationGen (Either Text))
exValidationGen = UserShape3
  (ValidationGen . Compose $ \t -> if t == "Carlo" then Right t else Left "Incorrect name")
  (ValidationGen . Compose $ \a -> if a < 100      then Right a else Left "No cyborgs here")
  (ValidationGen . Compose $ \d -> if d > 0        then Right d else Left "Positives values only")

-- And be able to call the generic validation function in this way:
exError :: UserShape3 Maybe
exError = validateRecord exUser3 exValidation3

exErrorGen :: UserShape3 (Either Text)
exErrorGen = validateRecordGen exUser3 exValidationGen

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

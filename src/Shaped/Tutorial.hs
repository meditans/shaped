{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, OverloadedStrings, RankNTypes   #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators               #-}
{-# LANGUAGE UndecidableInstances                                          #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Shaped.Tutorial where

import Data.Text
import Generics.SOP hiding (Compose)
import qualified GHC.Generics as GHC
import Shaped

import Data.Functor.Compose
import Data.Functor.Classes
import Control.Monad.Identity

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- User with only one field, for the tutorial
--------------------------------------------------------------------------------

data User = User { userMail :: Text } deriving (Show, Read, Eq, Ord, GHC.Generic)
data UserF f = UserF { userMailF :: f Text } deriving (GHC.Generic)
instance Generic User
instance Generic (UserF f)
instance (Show1 f) => Show (UserF f) where
  showsPrec d (UserF x) = showsUnaryWith showsPrec1 "UserF" d x

exUser = User "carlo@gmai.co"
exUserF, exUserF' :: UserF Identity
exUserF = UserF "Carlo"
exUserF' = UserF "no"

exValidationF :: ValidationF Text
exValidationF = ValidationF $ Compose (\a -> if a == "Carlo" then Just a else Nothing)

--------------------------------------------------------------------------------
-- User with two fields, for the tutorial
--------------------------------------------------------------------------------

data User2 = User2 { userMail2 :: Text, userAge2 :: Int } deriving (Show, Read, Eq, Ord, GHC.Generic)
data User2F f = User2F { userMail2F :: f Text, userAge2F :: f Int } deriving (GHC.Generic)
instance Generic User2
instance Generic (User2F f)
instance (Show1 f) => Show (User2F f) where
  showsPrec d (User2F x y) = showsBinaryWith showsPrec1 showsPrec1 "User2F" d x y

exUser2 = User2 "carlo@gmai.co" 45
exUser2F, exUser2F' :: User2F Identity
exUser2F = User2F "Carlo" 26
exUser2F' = User2F "no" 45

exValidationAge :: ValidationF Int
exValidationAge = ValidationF $ Compose (\a -> if a == 26 then Just a else Nothing)

--------------------------------------------------------------------------------
-- Various declinations of the validation functor
--------------------------------------------------------------------------------

newtype ValidationF a = ValidationF { unValidationF :: Compose ((->) a) Maybe a } deriving (GHC.Generic)
instance Generic (ValidationF a)

liftedValFun :: (ValidationF -.-> Identity -.-> Maybe) a
liftedValFun = fn_2 (\(ValidationF (Compose f)) (Identity a) -> f a)

liftedValFunPOP :: (SListI a, SListI2 a) => POP (ValidationF -.-> Identity -.-> Maybe) a
liftedValFunPOP = hpure liftedValFun

gvalidate :: UserF Identity -> UserF Maybe
gvalidate uI = to . toSOPI $ hliftA2 (\(ValidationF (Compose f)) (Identity a) -> f a) (simpleValidation exValidationF) idTrans
  where
    idTrans :: SOP Identity '[ '[Text] ]
    idTrans = fromSOPI $ from uI

gvalidate2 :: User2F Identity -> User2F Maybe
gvalidate2 uI = to . toSOPI $ hliftA2 (\(ValidationF (Compose f)) (Identity a) -> f a) (simpleValidation2 exValidationF exValidationAge ) idTrans
  where
    idTrans :: SOP Identity '[ '[Text, Int] ]
    idTrans = fromSOPI $ from uI

-- -- Only as a guide
-- trueValTrans :: POP ValidationF '[ '[Text] ]
-- trueValTrans = fromPOPI $ POP $ (I exValidationF :* Nil) :* Nil

simpleValidation :: ValidationF Text -> POP ValidationF '[ '[Text] ]
simpleValidation f = fromPOPI $ POP $ (I f :* Nil) :* Nil

simpleValidation2 :: ValidationF Text -> ValidationF Int -> POP ValidationF '[ '[Text, Int] ]
simpleValidation2 f g = fromPOPI $ POP $ (I f :* I g :* Nil) :* Nil


type NiceValidation xss = POP ValidationF xss

{-
the point would be constructing validations serving one validation function for each field, if they are there
So for a datatype A Int | B String we should give something like

validationForInt ::: validationForString

Eventually I want to construct a POP ValidationF xss from this validation. I
also can retrieve the [[]] from the original type.

For example, `:kind! Code User` is '['[Text]]

so I want only a ValidationAddend '[Text]

if the code were '[ '[Text], '[String] ]
I would want a `ValidationAddend '[ Text ]` and a `ValidationAddend '[ String ]`

How would I to specify this function?
-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Old UserShaped stuff, to inform the general vision
--------------------------------------------------------------------------------

-- -- Non vorrei definire questo
-- deriving instance Eq   (Field i Text) => Eq   (UserShaped i)
-- deriving instance Show (Field i Text) => Show (UserShaped i)

-- -- Non vorrei definire questo
-- instance Shaped User (UserShaped Literal) where
--   toShape   (User m) = UserShaped m
--   fromShape (UserShaped m) = User m

-- instance Shaped' User UserShaped where
--   toShape' (User m) = UserShaped m
--   fromShape' (UserShaped m) = User m

-- -- Questo dovrebbe avere una versione generale
-- validateUserWith :: User -> UserShaped Validation -> UserShaped Error
-- validateUserWith u uv = validateUser' (toShape u) uv
--   where
--     validateUser' :: UserShaped Literal -> UserShaped Validation -> UserShaped Error
--     validateUser' (UserShaped m) (UserShaped f) = UserShaped (f m)

-- -- Non vorrei definire questo
-- data UserShaped i = UserShaped
--   { userShapedMail :: Field i Text }  deriving (GHC.Generic)

-- instance Generic (UserShaped Error)
-- instance Generic (UserShaped Validation)

-- exUserError :: UserShaped Error
-- exUserError = UserShaped (Just "hey")

-- exUserValid :: UserShaped Validation
-- exUserValid = UserShaped (\t -> Just t)

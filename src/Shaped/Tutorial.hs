{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, OverloadedStrings, RankNTypes   #-}
{-# LANGUAGE StandaloneDeriving, TypeFamilies, TypeOperators               #-}
{-# LANGUAGE UndecidableInstances                                          #-}

{-# LANGUAGE ScopedTypeVariables #-}

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
-- User with only one field, for the tutorial
--------------------------------------------------------------------------------

data User = User { userMail :: Text } deriving (Show, Read, Eq, Ord, GHC.Generic)
data UserF f = UserF { userMailF :: f Text } deriving (GHC.Generic)
instance Generic User
instance Generic (UserF f)
instance (Show1 f) => Show (UserF f) where
  showsPrec d (UserF x) = showsUnaryWith showsPrec1 "UserF" d x

instance Shaped User UserF where
  toShape   (User m) = UserF (Identity m)
  fromShape (UserF (Identity m)) = User m

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

data UserShape2 f = UserShape2 (f Text) (f Int) deriving (GHC.Generic)

instance Generic User2
instance Generic (UserShape2 f)

instance (Show1 f) => Show (UserShape2 f) where
  showsPrec d (UserShape2 x y) = showsBinaryWith showsPrec1 showsPrec1 "UserShape2" d x y

-- TODO add a general Show instance for this kind of visualization!

instance Shaped User2 UserShape2 where
  toShape   (User2 n a) = UserShape2 (Identity n) (Identity a)
  fromShape (UserShape2 (Identity n) (Identity a)) = User2 n a

exUser2 = User2 "Carlo" 45

exValidationAge :: ValidationF Int
exValidationAge = ValidationF $ Compose (\a -> if a == 26 then Just a else Nothing)

exValidation2 :: UserShape2 ValidationF
exValidation2 = UserShape2 exValidationF exValidationAge

--------------------------------------------------------------------------------
-- Compact example for 3 fields, to note all the things needed
--------------------------------------------------------------------------------

data User3 = User3 { userName3 :: Text, userAge3 :: Int, userDouble3 :: Double }
  deriving (Show, Read, Eq, Ord, GHC.Generic)
data UserShape3 f = UserShape3 (f Text) (f Int) (f Double)
  deriving (GHC.Generic)

instance Generic User3
instance Generic (UserShape3 f)

instance Shaped User3 UserShape3 where
  toShape   (User3 n a d) = UserShape3 (Identity n) (Identity a) (Identity d)
  fromShape (UserShape3 (Identity n) (Identity a) (Identity d)) = User3 n a d

exUser3 = User3 "Carlo" 26 0.345

exValidation3 :: UserShape3 ValidationF
exValidation3 = UserShape3
  (ValidationF . Compose $ \t -> if t == "Carlo" then Just t else Nothing)
  (ValidationF . Compose $ \a -> if a < 100      then Just a else Nothing)
  (ValidationF . Compose $ \d -> if d > 0        then Just d else Nothing)

exError :: UserShape3 Maybe
exError = validateRecord exUser3 exValidation3

--------------------------------------------------------------------------------
-- Various declinations of the validation functor
--------------------------------------------------------------------------------

newtype ValidationF a = ValidationF { unValidationF :: Compose ((->) a) Maybe a } deriving (GHC.Generic)
instance Generic (ValidationF a)

validateRecord
  :: forall a s c c1 .
     ( Shaped a s, c ~ Code a, c ~ '[c1], SListI2 c
     , Code (s Identity)    ~ Map2 Identity c
     , Code (s Maybe)       ~ Map2 Maybe c
     , Code (s ValidationF) ~ Map2 ValidationF c
     , Generic (s Maybe)
     , Generic (s Identity)
     , Generic (s ValidationF))
  => a -> s ValidationF -> s Maybe
validateRecord u v = to . toSOPI
  $ hliftA2 (\(ValidationF (Compose f)) (Identity a) -> f a) vPOP uSOP
  where
    uSOP :: SOP Identity c
    uSOP =  fromSOPI . from $ toShape u
    vPOP :: POP ValidationF c
    vPOP = singleSOPtoPOP . fromSOPI $ from v

singleSOPtoPOP :: SOP f (xs ': '[]) -> POP f (xs ': '[])
singleSOPtoPOP (SOP (Z x))= POP (x :* Nil)

-- type NiceValidation xss = POP ValidationF xss

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

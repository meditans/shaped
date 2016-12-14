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
import Control.Monad.Identity

data User = User { userMail :: Text } deriving (Show, Read, Eq, Ord, GHC.Generic)

data UserF f = UserF { userMailF :: f Text } deriving (GHC.Generic)

instance Generic User
instance Generic (UserF f)

exUser = User "carlo@gmai.co"

-- Non vorrei definire questo
data UserShaped i = UserShaped
  { userShapedMail :: Field i Text }  deriving (GHC.Generic)

instance Generic (UserShaped Error)
instance Generic (UserShaped Validation)

exUserError :: UserShaped Error
exUserError = UserShaped (Just "hey")

exUserErrorF :: UserF Maybe
exUserErrorF = UserF (Just "hey")

exUserErrorSOPF :: SOP Maybe '[ '[Text] ]
exUserErrorSOPF = fromSOPI $ from exUserError

exUserErrorFSOPF :: SOP Maybe '[ '[Text] ]
exUserErrorFSOPF = fromSOPI $ from exUserErrorF

exUserValid :: UserShaped Validation
exUserValid = UserShaped (\t -> Just t)

exUserValidF :: UserF ValidationF
exUserValidF = UserF $ ValidationF $ Compose (\t -> Just t)

exUserValidFSOPF :: SOP ValidationF '[ '[Text] ]
exUserValidFSOPF = fromSOPI $ from exUserValidF

-- hpure :: SListIN h xs => (forall a. f a) -> h f xs
-- hap, ap_SOP :: POP (f -.-> g) xss -> SOP f xss -> SOP g xss

-- exUserValidSOPF :: SOP (Field Validation) '[ '[Text] ]
-- exUserValidSOPF = fromSOPI $ from exUserValid

liftedValFun :: (ValidationF -.-> Identity -.-> Maybe) a
liftedValFun = fn_2 (\(ValidationF (Compose f)) (Identity a) -> f a)

liftedValFunPOP :: (SListI a, SListI2 a) => POP (ValidationF -.-> Identity -.-> Maybe) a
liftedValFunPOP = hpure liftedValFun

gvalidate :: UserF Identity -> UserF ValidationF -> _ -- UserF Maybe
gvalidate uI uV = hliftA2 (\(ValidationF (Compose f)) (Identity a) -> f a) trueValTrans idTrans
  where
    idTrans :: SOP Identity '[ '[Text] ]
    idTrans= fromSOPI $ from uI
    valTrans :: SOP ValidationF '[ '[Text] ]
    valTrans= fromSOPI $ from uV
    trueValTrans :: POP ValidationF '[ '[Text] ]
    trueValTrans = undefined

newtype ValidationF a = ValidationF { unValidationF :: Compose ((->) a) Maybe a } deriving (GHC.Generic)
instance Generic (ValidationF a)

-- Non vorrei definire questo
deriving instance Eq   (Field i Text) => Eq   (UserShaped i)
deriving instance Show (Field i Text) => Show (UserShaped i)

-- Non vorrei definire questo
instance Shaped User (UserShaped Literal) where
  toShape   (User m) = UserShaped m
  fromShape (UserShaped m) = User m

instance Shaped' User UserShaped where
  toShape' (User m) = UserShaped m
  fromShape' (UserShaped m) = User m

-- Questo dovrebbe avere una versione generale
validateUserWith :: User -> UserShaped Validation -> UserShaped Error
validateUserWith u uv = validateUser' (toShape u) uv
  where
    validateUser' :: UserShaped Literal -> UserShaped Validation -> UserShaped Error
    validateUser' (UserShaped m) (UserShaped f) = UserShaped (f m)

--------------------------------------------------------------------------------

type family Map (f :: * -> *) (xs :: [*]) :: [*] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Map2 (f :: * -> *) (xs :: [[*]]) :: [[*]] where
  Map2 f '[]       = '[]
  Map2 f (x ': xs) = Map f x ': Map2 f xs

fromSOPI :: (zss ~ (xs ': xss), SListI zss, SListI2 zss)
         => SOP I (Map2 f zss) -> SOP f zss
fromSOPI (SOP x) = SOP (fromNSI x)

fromPOPI :: (SListI zss, SListI2 zss) => POP I (Map2 f zss) -> POP f zss
fromPOPI (POP x) = POP (fromNPI2 x)

fromNSI :: (zss ~ (xs ': xss), SListI zss, SListI2 zss)
        => NS (NP I) (Map2 f zss) -> NS (NP f) zss
fromNSI = go shape
  where
    go :: (zss ~ (xs ': xss), SListI zss, SListI2 zss)
       => Generics.SOP.Shape zss -> NS (NP I) (Map2 f zss) -> NS (NP f) zss
    go (ShapeCons ShapeNil)      (Z x)  = Z (fromNPI x)
    go (ShapeCons (ShapeCons _)) (S ns) = S (fromNSI ns)

fromNPI2 :: (SListI zss, SListI2 zss) => NP (NP I) (Map2 f zss) -> NP (NP f) zss
fromNPI2 = go shape
  where
    go :: (SListI zss, SListI2 zss)
       => Generics.SOP.Shape zss -> NP (NP I) (Map2 f zss) -> NP (NP f) zss
    go (ShapeNil)    (Nil)  = Nil
    go (ShapeCons _) (np :* nps) = fromNPI np :* fromNPI2 nps

fromNPI :: SListI xs => NP I (Map f xs) -> NP f xs
fromNPI = go sList
  where
    go :: SList ys -> NP I (Map f ys) -> NP f ys
    go SNil Nil = Nil
    go SCons (I x :* xs) = x :* go sList xs

toSOPI :: (zss ~ (xs ': xss), SListI zss, SListI2 zss)
       => SOP f zss -> SOP I (Map2 f zss)
toSOPI (SOP x) = SOP (toNSI x)

toPOPI :: (SListI zss, SListI2 zss)
       => POP f zss -> POP I (Map2 f zss)
toPOPI (POP x) = POP (toNPI2 x)

toNSI :: (zss ~ (xs ': xss), SListI zss, SListI2 zss)
        => NS (NP f) zss -> NS (NP I) (Map2 f zss)
toNSI = go shape
  where
    go :: (zss ~ (xs ': xss), SListI zss, SListI2 zss)
       => Generics.SOP.Shape zss -> NS (NP f) zss -> NS (NP I) (Map2 f zss)
    go (ShapeCons ShapeNil)      (Z x)  = Z (toNPI x)
    go (ShapeCons (ShapeCons _)) (S ns) = S (toNSI ns)

toNPI2 :: (SListI zss, SListI2 zss) => NP (NP f) zss -> NP (NP I) (Map2 f zss)
toNPI2 = go shape
  where
    go :: (SListI zss, SListI2 zss)
       => Generics.SOP.Shape zss -> NP (NP f) zss -> NP (NP I) (Map2 f zss)
    go (ShapeNil)      Nil  = Nil
    go (ShapeCons _) (np :* nps) = toNPI np :* toNPI2 nps

toNPI :: NP f xs -> NP I (Map f xs)
toNPI Nil = Nil
toNPI (fx :* np) = I fx :* toNPI np

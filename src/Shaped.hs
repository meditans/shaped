{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts                #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators          #-}

module Shaped where

import           Control.Monad.Identity
import           Data.Functor.Compose
import           Generics.SOP           hiding (Compose)
import qualified GHC.Generics           as GHC

-- The Shape type family is no longer used, in favour of simple (* -> *)
-- parametrization The problem with type families was that I have to completely
-- apply them, so I cannot talk about (Field Validation :: * -> *), which is
-- instead necessary for the usage in the generic approach.
-- This is the approach used now:
class Shaped (a :: *) (b :: (* -> *) -> *) | a -> b, b -> a where
  toShape   :: a -> b Identity
  fromShape :: b Identity -> a

-- A functor for validation. Note, this could be simplified:
newtype Validation a = Validation { unValidationF :: Compose ((->) a) Maybe a }
                     deriving (GHC.Generic)

instance Generic (Validation a)

-- A general function to check records
validateRecord
  :: forall a s c c1 .
     ( Shaped a s, c ~ Code a, c ~ '[c1], SListI2 c
     , Code (s Identity)    ~ Map2 Identity c
     , Code (s Maybe)       ~ Map2 Maybe c
     , Code (s Validation)  ~ Map2 Validation c
     , Generic (s Maybe)
     , Generic (s Identity)
     , Generic (s Validation))
  => a -> s Validation -> s Maybe
validateRecord u v = to . toSOPI
  $ hliftA2 (\(Validation (Compose f)) (Identity a) -> f a) vPOP uSOP
  where
    uSOP :: SOP Identity c
    uSOP =  fromSOPI . from $ toShape u
    vPOP :: POP Validation c
    vPOP = singleSOPtoPOP . fromSOPI $ from v

--------------------------------------------------------------------------------
-- SOP machinery
--------------------------------------------------------------------------------

---------------------------------------------------------- Mapping Type Families
type family Map (f :: * -> *) (xs :: [*]) :: [*] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Map2 (f :: * -> *) (xs :: [[*]]) :: [[*]] where
  Map2 f '[]       = '[]
  Map2 f (x ': xs) = Map f x ': Map2 f xs

------------------------------------------- X I (Map f xss) -> X f xss Functions
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
    go (ShapeNil)    (Nil)       = Nil
    go (ShapeCons _) (np :* nps) = fromNPI np :* fromNPI2 nps

fromNPI :: SListI xs => NP I (Map f xs) -> NP f xs
fromNPI = go sList
  where
    go :: SList ys -> NP I (Map f ys) -> NP f ys
    go SNil Nil          = Nil
    go SCons (I x :* xs) = x :* go sList xs

------------------------------------------- X f xss -> X I (Map f xss) Functions
toSOPI :: (zss ~ (xs ': xss), SListI zss, SListI2 zss)
       => SOP f zss -> SOP I (Map2 f zss)
toSOPI (SOP x) = SOP (toNSI x)

toPOPI :: (SListI zss, SListI2 zss) => POP f zss -> POP I (Map2 f zss)
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
    go (ShapeNil)      Nil       = Nil
    go (ShapeCons _) (np :* nps) = toNPI np :* toNPI2 nps

toNPI :: NP f xs -> NP I (Map f xs)
toNPI Nil        = Nil
toNPI (fx :* np) = I fx :* toNPI np

---------------------------------------------------- From SOP to POP for records

singleSOPtoPOP :: SOP f (xs ': '[]) -> POP f (xs ': '[])
singleSOPtoPOP (SOP (Z x)) = POP (x :* Nil)

------------------------------------------------------- Validation for sum types

{- TODO Add a way to specify validations for sum types.

the point would be constructing validations serving one validation function for
each field, if they are there So for a datatype A Int | B String we should give
something like

validationForInt ::: validationForString

Eventually I want to construct a POP Validation xss from this validation. I
also can retrieve the [[]] from the original type.

For example, `:kind! Code User` is '['[Text]]

so I want only a ValidationAddend '[Text]

if the code were '[ '[Text], '[String] ]
I would want a `ValidationAddend '[ Text ]` and a `ValidationAddend '[ String ]`
How would I to specify this function?
-}

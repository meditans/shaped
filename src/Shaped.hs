{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Shaped where

import Data.Text
import GHC.Generics
import Generics.SOP hiding (Compose)

-- The Shape type family is no longer used, in favour of simple (* -> *)
-- parametrization The problem with type families was that I have to completely
-- apply them, so I cannot talk about (Field Validation :: * -> *), which is
-- instead necessary for the usage in the generic approach.

-- data Shape = Literal | Error | Validation

-- type family Field i a where
--   Field Literal a    = a
--   Field Error a      = Maybe Text
--   Field Validation a = a -> Maybe Text

-- class Shaped a b | a -> b, b -> a where
--   toShape   :: a -> b
--   fromShape :: b -> a

-- class Shaped' (a :: *) (b :: Shape -> *) | a -> b, b -> a where
--   toShape'   :: a -> b Literal
--   fromShape' :: b Literal -> a

-- This should be a generic version:
-- validate :: (Shaped' a s) => a -> s Validation -> s Error
-- validate u uv = validate' (toShape u) uv
--   where
--     validateUser' :: UserShaped Literal -> UserShaped Validation -> UserShaped Error
--     validate' (s m) (s f) = UserShaped (f m)

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

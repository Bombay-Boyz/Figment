{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Figment.Check
  ( RequireGenericProvider
  , FieldsOf
  , OptionalFields
  , Elem
  ) where

import Data.Kind (Type, Constraint)
import GHC.Generics
  ( D1, C1, S1, Rec0, U1, (:*:), (:+:), Meta(..) )
import GHC.TypeLits (TypeError, ErrorMessage(..), Symbol)

import Figment.Provider
  ( EnvPrefixed, CliArgs, Defaults, FieldProvider )

type family IsGenericProvider (p :: Type) (r :: Type) :: Bool where
  IsGenericProvider EnvPrefixed         r = 'True
  IsGenericProvider CliArgs             r = 'True
  IsGenericProvider (Defaults r)        r = 'True
  IsGenericProvider (FieldProvider p)   r = 'True
  IsGenericProvider p                   r = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True  b = 'True
  Or 'False b = b

type family AnyGeneric (ps :: [Type]) (r :: Type) :: Bool where
  AnyGeneric '[]       r = 'False
  AnyGeneric (p ': ps) r = Or (IsGenericProvider p r) (AnyGeneric ps r)

type family RequireGenericProviderC (b :: Bool) :: Constraint where
  RequireGenericProviderC 'True  = ()
  RequireGenericProviderC 'False =
    TypeError
      ('Text "Figment: no provider in this chain can supply a field that "
       ':<>: 'Text "has no file-based value - add EnvPrefixed, CliArgs, "
       ':<>: 'Text "Defaults, or wrap a custom provider in FieldProvider.")

type RequireGenericProvider (r :: Type) (ps :: [Type]) =
  RequireGenericProviderC (AnyGeneric ps r)

type family FieldsOf (rep :: Type -> Type) :: [Symbol] where
  FieldsOf (D1 _ f)  = FieldsOf f
  FieldsOf (C1 _ f)  = FieldsOf f
  FieldsOf (f :*: g) = FieldsOf f ++ FieldsOf g
  FieldsOf U1        = '[]
  FieldsOf (S1 ('MetaSel ('Just name) _ _ _) _) = '[name]
  FieldsOf (S1 ('MetaSel 'Nothing _ _ _) _) =
    TypeError ('Text "Figment: config records must use named fields.")
  FieldsOf (f :+: g) =
    TypeError ('Text "Figment: config records must have exactly one constructor.")

type family OptionalFields (rep :: Type -> Type) :: [Symbol] where
  OptionalFields (D1 _ f)  = OptionalFields f
  OptionalFields (C1 _ f)  = OptionalFields f
  OptionalFields (f :*: g) = OptionalFields f ++ OptionalFields g
  OptionalFields U1        = '[]
  OptionalFields (S1 ('MetaSel ('Just name) _ _ _) (Rec0 (Maybe a))) = '[name]
  OptionalFields (S1 _ _)  = '[]

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Elem (k :: Symbol) (ks :: [Symbol]) :: Bool where
  Elem k '[]       = 'False
  Elem k (k ': ks) = 'True
  Elem k (j ': ks) = Elem k ks

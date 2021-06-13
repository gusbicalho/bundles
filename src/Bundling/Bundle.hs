{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.Bundle (
  Bundle (..),
  BundleSig (..),
  SomeBundle (..),
  someBundle,
  GetExport,
  getExport,
  MorphBundle,
  morph,
) where

import Bundling.TypeMap (TypeMap, TypeMapEntry (..), Merge)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError)
import Data.Proxy (Proxy(Proxy))

type Bundle :: Symbol -> Symbol -> TypeMap -> Type
data Bundle name owner exports where
  Bundle :: Bundle name owner '[]
  Exporting ::
    forall exportName export name owner exports.
    export ->
    Bundle name owner exports ->
    Bundle name owner (exportName ':-> export : exports)
  Skipping ::
    forall exportName export name owner exports.
    Bundle name owner exports ->
    Bundle name owner (exportName ':-> export : exports)

data BundleSig where
  BundleSig :: Symbol -> Symbol -> TypeMap -> BundleSig

type GetExport :: Symbol -> Type -> Type -> Constraint
class GetExport exportName export bundle where
  getExport :: bundle -> Maybe export

instance GetExport exportName export (Bundle name owner '[]) where
  getExport _ = Nothing

type CheckExportType :: Symbol -> Symbol -> Symbol -> Type -> Type -> Constraint
type family CheckExportType bundleName bundleOwner exportName requiredType actualType where
  CheckExportType _ _ _ t t = t ~ t
  CheckExportType bundleName bundleOwner exportName requiredType actualType =
    TypeError
      ( 'Text "Required export " ':<>: 'Text exportName ':<>: 'Text " with type:"
          ':$$: 'ShowType requiredType
          ':$$: ( 'Text "However, bundle " ':<>: 'Text bundleName
                    ':<>: 'Text ", owned by "
                    ':<>: 'Text bundleOwner
                    ':<>: 'Text ", exports "
                    ':<>: 'Text exportName
                    ':<>: 'Text " with type:"
                )
          ':$$: 'ShowType actualType
      )

type ExportTypeEq :: Symbol -> Symbol -> Symbol -> Type -> Type -> Constraint
class
  (requiredExport ~ actualExport) =>
  ExportTypeEq name owner exportName requiredExport actualExport
instance
  ( CheckExportType name owner exportName requiredExport actualExport
  , requiredExport ~ actualExport
  ) =>
  ExportTypeEq name owner exportName requiredExport actualExport

instance
  {-# OVERLAPPING #-}
  (ExportTypeEq name owner exportName requiredExport actualExport) =>
  GetExport exportName requiredExport (Bundle name owner (exportName ':-> actualExport ': exs))
  where
  getExport (Exporting export _) = Just export
  getExport (Skipping _) = Nothing

instance
  {-# OVERLAPPABLE #-}
  (GetExport exportName export (Bundle name owner exs)) =>
  GetExport exportName export (Bundle name owner (exportName' ':-> export' ': exs))
  where
  getExport (Exporting _ bundle) = getExport @exportName @export bundle
  getExport (Skipping bundle) = getExport @exportName @export bundle

type SomeBundle :: TypeMap -> Type
data SomeBundle requiredExports where
  SomeBundle ::
    forall exports (skips :: TypeMap) name owner.
    ( KnownSymbol name
    , KnownSymbol owner
    ) =>
    Bundle name owner exports ->
    Proxy skips ->
    SomeBundle (Merge exports skips)

instance
  (forall name owner. GetExport exportName export (Bundle name owner exports)) =>
  GetExport exportName export (SomeBundle exports)
  where
  getExport (SomeBundle bundle (_ :: p skips)) = getExport @exportName @export bundle

type MorphBundle :: (TypeMap -> Type) -> TypeMap -> TypeMap -> Constraint
class MorphBundle bundleC fromExports toExports where
  morphBundle :: bundleC fromExports -> bundleC toExports

instance MorphBundle (Bundle name owner) fromExports '[] where
  {-# INLINE morphBundle #-}
  morphBundle _ = Bundle @name @owner

instance
  ( MorphBundle (Bundle name owner) fromExports toMoreExports
  , GetExport exportName export (Bundle name owner fromExports)
  ) =>
  MorphBundle (Bundle name owner) fromExports (exportName ':-> export ': toMoreExports)
  where
  {-# INLINE morphBundle #-}
  morphBundle bundle =
    case getExport @exportName bundle of
      Just export -> base & Exporting @exportName export
      Nothing -> base & Skipping @exportName
   where
    base = morphBundle @_ @fromExports @toMoreExports bundle

instance MorphBundle SomeBundle fromExports '[] where
  {-# INLINE morphBundle #-}
  morphBundle (SomeBundle (_ :: Bundle name owner exports) (_ :: p skips)) =
    SomeBundle (Bundle @name @owner) (Proxy @'[])

-- instance
--   ( forall name owner. MorphBundle (Bundle name owner) fromExports toMoreExports
--   , forall name owner. GetExport exportName export (Bundle name owner fromExports)
--   ) =>
--   MorphBundle SomeBundle fromExports (exportName ':-> export ': toMoreExports)
--   where
--   {-# INLINE morphBundle #-}
--   morphBundle (SomeBundle bundle (__ :: p skips)) =
--     case morphBundle @_ @fromExports @toMoreExports bundle of
--       base ->
--         case getExport @exportName @export bundle of
--           Just export -> SomeBundle $ base & Exporting @exportName export
--           Nothing -> SomeBundle $ base & Skipping @exportName

{-# INLINE morph #-}
morph ::
  forall toExports fromExports bundleC.
  (MorphBundle bundleC fromExports toExports) =>
  bundleC fromExports ->
  bundleC toExports
morph = morphBundle @_ @fromExports @toExports

someBundle ::
  forall exports name owner bundleExports.
  ( KnownSymbol name
  , KnownSymbol owner
  , MorphBundle SomeBundle bundleExports exports
  ) =>
  Bundle name owner bundleExports ->
  SomeBundle exports
someBundle bundle = morph @exports $ SomeBundle bundle (Proxy @'[])

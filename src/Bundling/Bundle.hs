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
  getExportValue,
  getExport,
  getExportMaybe,
) where

import Bundling.TypeMap (KMapEntry (..), TypeMap)
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError)

type Bundle :: Symbol -> Symbol -> TypeMap -> Type
data Bundle name owner exports where
  Bundle :: Bundle name owner '[]
  Exporting ::
    forall exportName export name owner exports.
    export ->
    Bundle name owner exports ->
    Bundle name owner (exportName ':-> export : exports)

data BundleSig where
  BundleSig :: Symbol -> Symbol -> TypeMap -> BundleSig

data Optionality = Optional | Required

type OptionalityWrapped :: Optionality -> Type -> Type
type family OptionalityWrapped cardty t where
  OptionalityWrapped 'Optional t = Maybe t
  OptionalityWrapped 'Required t = t

type GetExport :: Symbol -> Type -> Optionality -> Type -> Constraint
class GetExport exportName export cardty bundle where
  getExportValue :: bundle -> OptionalityWrapped cardty export

instance GetExport exportName export 'Optional (Bundle name owner '[]) where
  getExportValue _ = Nothing

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
  GetExport exportName requiredExport 'Optional (Bundle name owner (exportName ':-> actualExport ': exs))
  where
  getExportValue (Exporting export _) = Just export

instance
  {-# OVERLAPPING #-}
  (ExportTypeEq name owner exportName requiredExport actualExport) =>
  GetExport exportName requiredExport 'Required (Bundle name owner (exportName ':-> actualExport ': exs))
  where
  getExportValue (Exporting export _) = export

instance
  {-# OVERLAPPABLE #-}
  (GetExport exportName export 'Required (Bundle name owner exs)) =>
  GetExport exportName export 'Required (Bundle name owner (exportName' ':-> export' ': exs))
  where
  getExportValue (Exporting _ bundle) = getExportValue @exportName @export @ 'Required bundle

instance
  {-# OVERLAPPABLE #-}
  (GetExport exportName export 'Optional (Bundle name owner exs)) =>
  GetExport exportName export 'Optional (Bundle name owner (exportName' ':-> export' ': exs))
  where
  getExportValue (Exporting _ bundle) = getExportValue @exportName @export @ 'Optional bundle

data SomeBundle where
  SomeBundle ::
    ( KnownSymbol name
    , KnownSymbol owner
    , forall exportName export. GetExport exportName export 'Optional (Bundle name owner exports)
    ) =>
    Bundle name owner exports ->
    SomeBundle

instance GetExport exportName export 'Optional SomeBundle where
  getExportValue (SomeBundle bundle) = getExportValue @exportName @export @ 'Optional bundle

someBundle ::
  ( KnownSymbol name
  , KnownSymbol owner
  , forall exportName export. GetExport exportName export 'Optional (Bundle name owner exports)
  ) =>
  Bundle name owner exports ->
  SomeBundle
someBundle = SomeBundle

getExport ::
  forall (exportName :: Symbol) export bundle.
  (GetExport exportName export 'Required bundle) =>
  bundle ->
  export
getExport = getExportValue @exportName @export @ 'Required

getExportMaybe ::
  forall (exportName :: Symbol) export bundle.
  (GetExport exportName export 'Optional bundle) =>
  bundle ->
  Maybe export
getExportMaybe = getExportValue @exportName @export @ 'Optional

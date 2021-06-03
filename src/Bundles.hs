{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- |
Copyright: (c) 2021 Gustavo Bicalho
SPDX-License-Identifier: MIT
Maintainer: Gustavo Bicalho <gusbicalho@gmail.com>

See README for more info
-}
module Bundles where

import Data.Function ((&))
import Data.Kind (Constraint, Type)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError)

-- type (:->) :: Type -> Type -> (Type, Type)
data TypeMapEntry = Symbol :-> Type

type TypeMap = [TypeMapEntry]

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

type MorphBundle :: TypeMap -> TypeMap -> Constraint
class MorphBundle fromExports toExports where
  morphBundle :: Bundle name owner fromExports -> Bundle name owner toExports

instance MorphBundle fromExports '[] where
  morphBundle (_ :: Bundle name owner fromExports) = Bundle @name @owner

instance MorphBundle exports exports where
  morphBundle bundle = bundle

instance
  ( MorphBundle fromExports toMoreExports
  , forall name owner. GetExport exportName export (Bundle name owner fromExports)
  ) =>
  MorphBundle fromExports (exportName ':-> export ': toMoreExports)
  where
  morphBundle bundle =
    case getExport @exportName bundle of
      Just export -> base & Exporting @exportName export
      Nothing -> base & Skipping @exportName
   where
    base = morphBundle @fromExports @toMoreExports bundle

morph ::
  forall toExports fromExports name owner.
  (MorphBundle fromExports toExports) =>
  Bundle name owner fromExports ->
  Bundle name owner toExports
morph = morphBundle @fromExports @toExports

type SomeBundle :: TypeMap -> Type
data SomeBundle requiredExports where
  SomeBundle ::
    forall exports name owner.
    ( KnownSymbol name
    , KnownSymbol owner
    ) =>
    Bundle name owner exports ->
    SomeBundle exports

someBundle ::
  forall exports name owner bundleExports.
  ( KnownSymbol name
  , KnownSymbol owner
  , MorphBundle bundleExports exports
  ) =>
  Bundle name owner bundleExports ->
  SomeBundle exports
someBundle = SomeBundle . morph

type BundleFactory :: Symbol -> Symbol -> TypeMap -> TypeMap -> Type
data BundleFactory name owner imports exports where
  BundleFactory ::
    forall name owner imports exports.
    ([SomeBundle imports] -> [SomeBundle exports]) ->
    BundleFactory name owner imports exports

bundleA ::
  Bundle
    "bundleA"
    "red"
    '["bar/name" ':-> Text, "foo/val" ':-> Word]
bundleA =
  Bundle @"bundleA" @"red"
    & Exporting @"foo/val" @Word 42
    & Exporting @"bar/name" @Text "Pretty foolish"

factoryFoo ::
  BundleFactory
    "factoryFoo"
    "blue"
    '["foo/val" ':-> Word, "foo/name" ':-> Text]
    '["bar/total" ':-> Word, "bar/names" ':-> [Text]]
factoryFoo = BundleFactory $ \inputBundles ->
  let total = inputBundles & Maybe.mapMaybe (\(SomeBundle b) -> getExport @"foo/val" @Word b) & sum
      names = inputBundles & Maybe.mapMaybe (\(SomeBundle b) -> getExport @"foo/name" @Text b)
   in [ someBundle $
          Bundle @"factoryFoo.bundle" @"blue"
            & Exporting @"bar/total" total
            & Exporting @"bar/names" names
      ]

runFactory :: BundleFactory name owner imports exports -> [SomeBundle imports] -> [SomeBundle exports]
runFactory (BundleFactory runF) = runF

-- b = getExport @"foo/val" @Int a

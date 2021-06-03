{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

type SomeBundle :: TypeMap -> Type
data SomeBundle requiredExports where
  SomeBundle ::
    forall exports name owner.
    ( KnownSymbol name
    , KnownSymbol owner
    ) =>
    Bundle name owner exports ->
    SomeBundle exports

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
  morphBundle (SomeBundle (_ :: Bundle name owner exports)) = SomeBundle $ Bundle @name @owner

instance
  ( forall name owner. MorphBundle (Bundle name owner) fromExports toMoreExports
  , forall name owner. GetExport exportName export (Bundle name owner fromExports)
  ) =>
  MorphBundle SomeBundle fromExports (exportName ':-> export ': toMoreExports)
  where
  {-# INLINE morphBundle #-}
  morphBundle (SomeBundle bundle) =
    case morphBundle @_ @fromExports @toMoreExports bundle of
      base ->
        case getExport @exportName @export bundle of
          Just export -> SomeBundle $ base & Exporting @exportName export
          Nothing -> SomeBundle $ base & Skipping @exportName

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
  , MorphBundle (Bundle name owner) bundleExports exports
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

data BundleFactorySig where
  BundleFactorySig :: Symbol -> Symbol -> TypeMap -> TypeMap -> BundleFactorySig

data BundleSig where
  BundleSig :: Symbol -> Symbol -> TypeMap -> BundleSig

type Bundles :: [BundleSig] -> Type
data Bundles signatures where
  NoBundles :: Bundles '[]
  AddBundle ::
    Bundle name owner exports ->
    Bundles sigs ->
    Bundles ( 'BundleSig name owner exports : sigs)

type Factories :: [BundleFactorySig] -> Type
data Factories signatures where
  NoFactories :: Factories '[]
  AddFactory ::
    BundleFactory name owner imports exports ->
    Factories sigs ->
    Factories ( 'BundleFactorySig name owner imports exports : sigs)

type BundlingSetup :: [BundleSig] -> [BundleFactorySig] -> Type
data BundlingSetup bundles factories where
  BundlingSetup :: Bundles bundles -> Factories factories -> BundlingSetup bundles factories

emptySetup :: BundlingSetup '[] '[]
emptySetup = BundlingSetup NoBundles NoFactories

addBundle ::
  Bundle name owner exports ->
  BundlingSetup bundles factories ->
  BundlingSetup ( 'BundleSig name owner exports : bundles) factories
addBundle bundle (BundlingSetup bundles factories) =
  BundlingSetup (bundles & AddBundle bundle) factories

addFactory ::
  BundleFactory name owner imports exports ->
  BundlingSetup bundles factories ->
  BundlingSetup bundles ( 'BundleFactorySig name owner imports exports : factories)
addFactory factory (BundlingSetup bundles factories) =
  BundlingSetup bundles (factories & AddFactory factory)

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

assembled :: BundlingSetup _ _
assembled =
  emptySetup
    & addBundle bundleA
      . addFactory factoryFoo

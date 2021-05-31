{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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

instance
  {-# OVERLAPPING #-}
  ( CheckExportType name owner exportName requiredExport actualExport
  , requiredExport ~ actualExport
  ) =>
  GetExport exportName requiredExport (Bundle name owner (exportName ':-> actualExport ': exs))
  where
  getExport (Exporting export _) = Just export

instance
  {-# OVERLAPPABLE #-}
  (GetExport exportName export (Bundle name owner exs)) =>
  GetExport exportName export (Bundle name owner (exportName' ':-> export' ': exs))
  where
  getExport (Exporting _ bundle) = getExport @exportName @export bundle

type GetExports :: TypeMap -> [Type -> Constraint]
type family GetExports exs where
  GetExports '[] = '[]
  GetExports (exportName ':-> export : exs) = (GetExport exportName export : GetExports exs)

type AllF :: [Type -> Constraint] -> Type -> Constraint
type family AllF cs t where
  AllF '[] t = ()
  AllF (c : cs) t = (c t, AllF cs t)

type All :: [Type -> Constraint] -> Type -> Constraint
class (AllF cs t) => All cs t
instance (AllF cs t) => All cs t

type SomeBundle :: TypeMap -> Type
data SomeBundle requiredExports where
  SomeBundle ::
    forall requiredExports name owner actualExports.
    ( All (GetExports requiredExports) (Bundle name owner actualExports)
    , KnownSymbol name
    , KnownSymbol owner
    ) =>
    Bundle name owner actualExports ->
    SomeBundle requiredExports

-- TODO implement loosen
loosenBundle :: SomeBundle exports -> SomeBundle (newExName ':-> newExType ': exports)
loosenBundle (SomeBundle b) = undefined -- SomeBundle b

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
   in [ SomeBundle $
          Bundle @"factoryFoo.bundle" @"blue"
            & Exporting @"bar/total" total
            & Exporting @"bar/names" names
      ]

runFactory :: BundleFactory name owner imports exports -> [SomeBundle imports] -> [SomeBundle exports]
runFactory (BundleFactory runF) = runF

-- b = getExport @"foo/val" @Int a

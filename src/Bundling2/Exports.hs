{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bundling2.Exports (
  ExportName (..),
  Export,
  ExportsMap,
  empty,
  addExport',
  addExport,
  getExport',
  getExport,
) where

import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

-- Defines Exports

type ExportName :: Type -> Constraint
class Typeable exportName => ExportName exportName where
  type ExportContent exportName :: Type

type Exports exportName exportContent =
  (ExportName exportName, ExportContent exportName ~ exportContent)

-- Helpers for defining exports

data Export t

instance Typeable t => ExportName (Export t) where
  type ExportContent (Export t) = t

-- Map of Exports

newtype ExportsMap = MkExportsMap (Map TypeRep Any)

instance Semigroup ExportsMap where
  MkExportsMap m1 <> MkExportsMap m2 = MkExportsMap $ Map.union m1 m2

instance Monoid ExportsMap where
  mempty = MkExportsMap Map.empty

empty :: ExportsMap
empty = mempty

addExport' ::
  Exports exportName t =>
  proxy exportName ->
  t ->
  ExportsMap ->
  ExportsMap
addExport' (label :: proxy exportName) v (MkExportsMap m) =
  MkExportsMap $
    Map.insert
      (typeRep label)
      (unsafeCoerce @(ExportContent exportName) @Any v)
      m

addExport ::
  forall exportName t.
  Exports exportName t =>
  t ->
  ExportsMap ->
  ExportsMap
addExport = addExport' (Proxy @exportName)

getExport' ::
  ExportName exportName =>
  proxy exportName ->
  ExportsMap ->
  Maybe (ExportContent exportName)
getExport' (label :: proxy exportName) (MkExportsMap m) =
  unsafeCoerce @Any @(ExportContent exportName)
    <$> Map.lookup (typeRep label) m

getExport ::
  forall exportName.
  ExportName exportName =>
  ExportsMap ->
  Maybe (ExportContent exportName)
getExport = getExport' (Proxy @exportName)

-- Examples

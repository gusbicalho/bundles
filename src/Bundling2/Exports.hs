{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling2.Exports (
  ExportName (..),
  Some (..),
  Is,
  ExportsMap,
  empty,
  addExport,
  getSomeExport,
) where

import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Base (Any)
import Unsafe.Coerce (unsafeCoerce)

class (a ~ b) => Is a b
instance (a ~ b) => Is a b

type ExportName :: Type -> Constraint
class Typeable exportName => ExportName exportName where
  type ExportContent exportName :: Type -> Constraint

type Exports exportName exportContent =
  (ExportName exportName, ExportContent exportName exportContent)

type Some :: (Type -> Constraint) -> Type
data Some c where
  Some :: c t => !t -> Some c

newtype ExportsMap = MkExportsMap (Map TypeRep (Some Any))

instance Semigroup ExportsMap where
  MkExportsMap m1 <> MkExportsMap m2 = MkExportsMap $ Map.union m1 m2

instance Monoid ExportsMap where
  mempty = MkExportsMap Map.empty

empty :: ExportsMap
empty = mempty

addExport ::
  forall exportName t.
  Exports exportName t =>
  Proxy exportName ->
  t ->
  ExportsMap ->
  ExportsMap
addExport label v (MkExportsMap m) =
  MkExportsMap $
    Map.insert
      (typeRep label)
      ( unsafeCoerce @(Some (ExportContent exportName)) @(Some Any) $
          Some v
      )
      m

getSomeExport ::
  forall exportName.
  ExportName exportName =>
  Proxy exportName ->
  ExportsMap ->
  Maybe (Some (ExportContent exportName))
getSomeExport label (MkExportsMap m) =
  unsafeCoerce @(Some Any) @(Some (ExportContent exportName))
    <$> Map.lookup (typeRep label) m

withExport ::
  ExportName exportName =>
  Proxy exportName ->
  ExportsMap ->
  (forall t. ExportContent exportName t => t -> p) ->
  Maybe p
withExport label exportsMap f =
  getSomeExport label exportsMap <&> \case
    Some t -> f t

getExport ::
  ( ExportName exportName
  , forall t. ExportContent exportName t => t ~ p
  ) =>
  Proxy exportName ->
  ExportsMap ->
  Maybe p
getExport label exports = withExport label exports id

{-

>>> data Foo
>>> instance ExportName Foo where type ExportContent Foo = Is Word
>>> data Bar
>>> instance ExportName Bar where type ExportContent Bar = Show
>>> b = addExport (Proxy @Bar) "abc" $ addExport (Proxy @Foo) 42 $ mempty
>>> getExport (Proxy @Foo) b
Just 42
>>> withExport (Proxy @Bar) b show
Just "\"abc\""
>>> getExport (Proxy @Bar) b
Couldn't match type ‘t’ with ‘p’ arising from a use of ‘getExport’
‘t’ is a rigid type variable bound by
  a quantified context
  at /home/gusbicalho/dev/bundles/src/Bundling2/Exports.hs:116:2-25
‘p’ is a rigid type variable bound by
  the inferred type of it :: Maybe p
  at /home/gusbicalho/dev/bundles/src/Bundling2/Exports.hs:116:2-25

-}

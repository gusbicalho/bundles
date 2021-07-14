{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
module Bundling where

import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Control.Arrow ((&&&))
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint, Type)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeRep)
import GHC.Base (Any)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import HList (HList (HNil, (:::)))
import Unsafe.Coerce (unsafeCoerce)
import Numeric.Natural (Natural)

type BundleMeta = String
type RepMap = [(TypeRep, Any)]

lookupT :: forall t. (Typeable t) => RepMap -> Maybe t
lookupT repMap =
  case lookup (typeRep (Proxy @t)) repMap of
    Nothing -> Nothing
    Just a -> Just (unsafeCoerce a :: t)

data Bundle = Bundle
  { bundleMeta :: BundleMeta
  , bundleExports :: RepMap
  }

data BundleTyped types = BundleTyped BundleMeta (HList types)

getExport :: forall t. (Typeable t) => Bundle -> Maybe t
getExport (Bundle _ exports) = lookupT @t exports

data FactorySpec
  = FactorySpec Symbol TypeSet TypeSet

newtype Factory (spec :: FactorySpec)
  = Factory ([Bundle] -> [Bundle])

class ToRepMap t where
  toRepMap :: t -> RepMap

instance ToRepMap (HList '[]) where
  toRepMap HNil = []

instance (Typeable t, ToRepMap (HList ts)) => ToRepMap (HList (t ': ts)) where
  toRepMap (a ::: more) =
    (typeRep (Proxy @t), unsafeCoerce a) : toRepMap more

class FromRepMap t where
  fromRepMap :: RepMap -> t

instance FromRepMap (HList '[]) where
  fromRepMap _ = HNil

instance (t ~ Maybe u, Typeable u, FromRepMap (HList ts)) => FromRepMap (HList (t ': ts)) where
  fromRepMap repMap = lookupT @u repMap ::: fromRepMap @(HList ts) repMap

standalone ::
  forall name types.
  (ToRepMap (HList types), KnownSymbol name) =>
  HList types ->
  Factory ( 'FactorySpec name TS.Empty (TS.FromList types))
standalone feats = Factory $ const [Bundle meta exports]
 where
  meta = symbolVal (Proxy @name) <> ".bundle"
  exports = toRepMap feats

type MapTypes :: forall k l. (k -> l) -> [k] -> [l]
type family MapTypes f types where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f t ': MapTypes f ts

factory ::
  forall name inputs outputs.
  ( FromRepMap (HList (MapTypes Maybe inputs))
  , ToRepMap (HList (MapTypes Maybe outputs))
  ) =>
  ([BundleTyped inputs] -> [BundleTyped outputs]) ->
  Factory ( 'FactorySpec name (TS.FromList inputs) (TS.FromList outputs))
factory runFactory = Factory go
 where
  go bundles = toBundle <$> runFactory (fromBundle <$> bundles)
  toBundle (BundleTyped meta exports) = Bundle meta (toRepMap exports)
  fromBundle Bundle{bundleMeta, bundleExports} = BundleTyped bundleMeta (fromRepMap bundleExports)

bla =
  factory
    @"foo"
    @'[Word, Char, String]
    @'[Natural, String, [String]]
    (build . Foldable.foldl' collect empty)
 where
  empty = ([], [], [], [])
  collect (metas, ws, cs, strs) _ = _
  build (metas, ws, cs, strs) = _

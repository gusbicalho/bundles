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
module Bundling () where

import Bundling.RepMap (RepMap)
import Bundling.RepMap qualified as RepMap
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import HList (HList)

type MapTypes :: forall k l. (k -> l) -> [k] -> [l]
type family MapTypes f types where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f t ': MapTypes f ts

type Maybes :: [Type] -> [Type]
type Maybes types = MapTypes Maybe types

data DynBundle meta = DynBundle
  { bundleMeta :: meta
  , bundleExports :: RepMap
  }

data Bundle meta types = Bundle meta (HList (Maybes types))

getExport :: forall t meta. (Typeable t) => DynBundle meta -> Maybe t
getExport (DynBundle _ exports) = RepMap.lookup @t exports

data FactorySpec
  = FactorySpec Symbol Type TypeSet TypeSet

type FactoryBundleMeta :: FactorySpec -> Type
type family FactoryBundleMeta spec where
  FactoryBundleMeta ( 'FactorySpec _ bundleMeta _ _) = bundleMeta

newtype Factory (spec :: FactorySpec)
  = Factory ([DynBundle (FactoryBundleMeta spec)] -> [DynBundle (FactoryBundleMeta spec)])

type AllUniqueTypes :: [Type] -> Constraint
type AllUniqueTypes types = types ~ TS.Elements (TS.FromList types)

type HListOfInputs t inputs =
  ( t ~ HList (Maybes inputs)
  , AllUniqueTypes inputs
  , RepMap.FromRepMap t
  )

type ValidInputs inputs = HListOfInputs (HList (Maybes inputs)) inputs

type HListOfOutputs t outputs =
  ( t ~ HList (Maybes outputs)
  , AllUniqueTypes outputs
  , RepMap.ToRepMap t
  )

type ValidOutputs outputs = HListOfOutputs (HList (Maybes outputs)) outputs

standalone ::
  forall name types bundleMeta output.
  (HListOfOutputs output types) =>
  bundleMeta ->
  output ->
  Factory ( 'FactorySpec name bundleMeta TS.Empty (TS.FromList types))
standalone meta exportsList = Factory $ const [DynBundle meta exports]
 where
  exports = RepMap.toRepMap exportsList

factory ::
  forall name inputs outputs bundleMeta.
  ( ValidInputs inputs
  , ValidOutputs outputs
  ) =>
  ([Bundle bundleMeta inputs] -> [Bundle bundleMeta outputs]) ->
  Factory ( 'FactorySpec name bundleMeta (TS.FromList inputs) (TS.FromList outputs))
factory runFactory = Factory go
 where
  go bundles = toBundle <$> runFactory (fromBundle <$> bundles)
  toBundle (Bundle meta exports) = DynBundle meta (RepMap.toRepMap exports)
  fromBundle DynBundle{bundleMeta, bundleExports} =
    Bundle bundleMeta (RepMap.fromRepMap bundleExports)

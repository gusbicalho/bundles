{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Bundling.Factory (
  Factory,
  FactorySpec (..),
  runFactory,
  addFromFactory,
  standalone,
  factory,
) where

import Bundling.Bundle (
  Bundle (..),
  DynamicBundle (..),
  HListOfOutputs,
  ValidInputs,
  ValidOutputs,
  dynamicToTyped,
  typedToDynamic,
 )
import Bundling.RepMap qualified as RepMap
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Type)
import Data.Maybe qualified as Maybe
import GHC.TypeLits (Symbol)

data FactorySpec
  = FactorySpec Symbol Type TypeSet TypeSet

type FactoryBundleMeta :: FactorySpec -> Type
type family FactoryBundleMeta spec where
  FactoryBundleMeta ( 'FactorySpec _ bundleMeta _ _) = bundleMeta

newtype Factory (spec :: FactorySpec) = Factory
  { runFactory :: [DynamicBundle (FactoryBundleMeta spec)] -> [DynamicBundle (FactoryBundleMeta spec)]
  }

standalone ::
  forall name types bundleMeta output.
  (HListOfOutputs output types) =>
  bundleMeta ->
  output ->
  Factory ( 'FactorySpec name bundleMeta TS.Empty (TS.FromList types))
standalone meta exportsList = Factory $ const [DynamicBundle meta exports]
 where
  exports = RepMap.toRepMap exportsList

factory ::
  forall name inputs outputs bundleMeta.
  ( ValidInputs inputs
  , ValidOutputs outputs
  ) =>
  ([Bundle bundleMeta inputs] -> [Bundle bundleMeta outputs]) ->
  Factory ( 'FactorySpec name bundleMeta (TS.FromList inputs) (TS.FromList outputs))
factory makeBundles = Factory go
 where
  go bundles = typedToDynamic <$> makeBundles (Maybe.mapMaybe dynamicToTyped bundles)

addFromFactory ::
  Factory spec ->
  [DynamicBundle (FactoryBundleMeta spec)] ->
  [DynamicBundle (FactoryBundleMeta spec)]
addFromFactory fac bundles = bundles <> runFactory fac bundles

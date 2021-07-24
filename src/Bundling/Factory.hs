{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.Factory (
  Factory,
  Factories,
  FactorySpec (..),
  ValidFactory,
  FactoryName,
  FactoryBundleMeta,
  FactoryInputs,
  FactoryOutputs,
  runFactory,
  addFromFactory,
  standalone,
  factory,
) where

import Bundling.Bundle (
  Bundle (..),
  DynamicBundle (..),
 )
import Bundling.Bundle qualified as Bundle
import Bundling.TypeSet (TypeSet)
import Bundling.TypeSet qualified as TS
import Data.Kind (Constraint, Type)
import Data.Maybe qualified as Maybe
import GHC.TypeLits (KnownSymbol, Symbol)

data FactorySpec
  = FactorySpec Symbol Type TypeSet TypeSet

type FactoryName :: FactorySpec -> Symbol
type family FactoryName spec where
  FactoryName ( 'FactorySpec name _ _ _) = name

type FactoryBundleMeta :: FactorySpec -> Type
type family FactoryBundleMeta spec where
  FactoryBundleMeta ( 'FactorySpec _ bundleMeta _ _) = bundleMeta

type FactoryInputs :: FactorySpec -> TypeSet
type family FactoryInputs spec where
  FactoryInputs ( 'FactorySpec _ _ inputs _) = inputs

type FactoryOutputs :: FactorySpec -> TypeSet
type family FactoryOutputs spec where
  FactoryOutputs ( 'FactorySpec _ _ _ outputs) = outputs

type ValidFactory spec =
  ( KnownSymbol (FactoryName spec)
  , Bundle.ValidInputs (FactoryInputs spec)
  , Bundle.ValidOutputs (FactoryOutputs spec)
  )

data Factory (spec :: FactorySpec) where
  Factory ::
    ( ValidFactory spec
    ) =>
    ( [Bundle (FactoryBundleMeta spec) (FactoryInputs spec)] ->
      [Bundle (FactoryBundleMeta spec) (FactoryOutputs spec)]
    ) ->
    Factory spec

type GoFactories :: [FactorySpec] -> [Type]
type family GoFactories specs = factories | factories -> specs where
  GoFactories '[] = '[]
  GoFactories (spec ': moreSpecs) = Factory spec ': GoFactories moreSpecs

type FactoriesHaveSameMeta :: Type -> [FactorySpec] -> Constraint
type family FactoriesHaveSameMeta bundleMeta specs where
  FactoriesHaveSameMeta meta '[] = ()
  FactoriesHaveSameMeta meta (spec ': specs) =
    ( meta ~ FactoryBundleMeta spec
    , FactoriesHaveSameMeta meta specs
    )

type Factories :: Type -> [FactorySpec] -> (Constraint, [Type])
type family Factories bundleMeta specs where
  Factories bundleMeta specs = '(FactoriesHaveSameMeta bundleMeta specs, GoFactories specs)

standalone ::
  forall name types bundleMeta spec.
  ( spec ~ 'FactorySpec name bundleMeta TS.Empty types
  , ValidFactory spec
  ) =>
  Bundle bundleMeta types ->
  Factory spec
standalone bundle = Factory $ const [bundle]

factory ::
  forall name inputs outputs bundleMeta spec.
  ( spec ~ 'FactorySpec name bundleMeta inputs outputs
  , ValidFactory spec
  ) =>
  ([Bundle bundleMeta inputs] -> [Bundle bundleMeta outputs]) ->
  Factory spec
factory = Factory

runFactory ::
  forall spec.
  (ValidFactory spec) =>
  Factory spec ->
  [DynamicBundle (FactoryBundleMeta spec)] ->
  [DynamicBundle (FactoryBundleMeta spec)]
runFactory (Factory makeBundles) bundles =
  Bundle.typedToDynamic <$> makeBundles (Maybe.mapMaybe Bundle.dynamicToTyped bundles)

addFromFactory ::
  forall spec.
  (ValidFactory spec) =>
  Factory spec ->
  [DynamicBundle (FactoryBundleMeta spec)] ->
  [DynamicBundle (FactoryBundleMeta spec)]
addFromFactory fac bundles = bundles <> runFactory fac bundles

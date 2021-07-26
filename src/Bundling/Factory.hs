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
  FactoriesHaveSameMeta,
  ValidFactory,
  ValidFactories,
  FactoryName,
  FactoryBundleMeta,
  FactoryInputs,
  FactoryOutputs,
  AllFactoryOutputs,
  runFactory,
  addFromFactory,
  standalone,
  factory,
  factoryPure,
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

data Factory (m :: Type -> Type) (spec :: FactorySpec) where
  Factory ::
    ( ValidFactory spec
    ) =>
    ( [Bundle (FactoryBundleMeta spec) (FactoryInputs spec)] ->
      m [Bundle (FactoryBundleMeta spec) (FactoryOutputs spec)]
    ) ->
    Factory m spec

type Factories :: (Type -> Type) -> [FactorySpec] -> [Type]
type family Factories m specs = factories | factories -> specs where
  Factories m '[] = '[]
  Factories m (spec ': moreSpecs) = Factory m spec ': Factories m moreSpecs

type FactoriesHaveSameMeta :: Type -> [FactorySpec] -> Constraint
type family FactoriesHaveSameMeta bundleMeta specs where
  FactoriesHaveSameMeta meta '[] = ()
  FactoriesHaveSameMeta meta (spec ': specs) =
    ( meta ~ FactoryBundleMeta spec
    , FactoriesHaveSameMeta meta specs
    )

type ValidFactories :: [FactorySpec] -> Constraint
type family ValidFactories specs where
  ValidFactories '[] = ()
  ValidFactories (spec ': specs) =
    ( ValidFactory spec
    , ValidFactories specs
    )

type AllFactoryOutputs :: [FactorySpec] -> TypeSet
type family AllFactoryOutputs specs where
  AllFactoryOutputs '[] = TS.Empty
  AllFactoryOutputs (f ': fs) = TS.Union (FactoryOutputs f) (AllFactoryOutputs fs)

standalone ::
  forall name m types bundleMeta spec.
  ( spec ~ 'FactorySpec name bundleMeta TS.Empty types
  , ValidFactory spec
  , Applicative m
  ) =>
  Bundle bundleMeta types ->
  Factory m spec
{-# INLINE standalone #-}
standalone = Factory . pure @((->) _) . pure @m . pure @[]
-- ^ a bundle, wrapped on a list, wrapped on m, wrapped on a const fn, wrapped on a Factory

factory ::
  forall name inputs outputs bundleMeta spec m.
  ( spec ~ 'FactorySpec name bundleMeta inputs outputs
  , ValidFactory spec
  ) =>
  ([Bundle bundleMeta inputs] -> m [Bundle bundleMeta outputs]) ->
  Factory m spec
{-# INLINE factory #-}
factory = Factory

factoryPure ::
  forall name inputs outputs bundleMeta spec m.
  ( spec ~ 'FactorySpec name bundleMeta inputs outputs
  , ValidFactory spec
  , Applicative m
  ) =>
  ([Bundle bundleMeta inputs] -> [Bundle bundleMeta outputs]) ->
  Factory m spec
{-# INLINE factoryPure #-}
factoryPure = factory . fmap pure

runFactory ::
  forall m spec.
  (ValidFactory spec, Functor m) =>
  Factory m spec ->
  [DynamicBundle (FactoryBundleMeta spec)] ->
  m [DynamicBundle (FactoryBundleMeta spec)]
{-# INLINE runFactory #-}
runFactory (Factory makeBundles) bundles =
  (Bundle.typedToDynamic <$>) <$> makeBundles (Maybe.mapMaybe Bundle.dynamicToTyped bundles)

addFromFactory ::
  forall m spec.
  (ValidFactory spec, Functor m) =>
  Factory m spec ->
  [DynamicBundle (FactoryBundleMeta spec)] ->
  m [DynamicBundle (FactoryBundleMeta spec)]
{-# INLINE addFromFactory #-}
addFromFactory fac bundles = (bundles <>) <$> runFactory fac bundles

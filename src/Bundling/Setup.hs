{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Bundling.Setup (
  Setup (..),
  assembleSetup,
  runSetup,
  BuildSetup,
  buildSetup,
) where

import Bundling.Assemble (Assemble)
import Bundling.Assemble qualified as Assemble
import Bundling.Bundle (DynamicBundle)
import Bundling.Factory (Factories, Factory, FactorySpec (..))
import Bundling.Factory qualified as Factory
import Bundling.Setup.PopNextReadyFactory (NextReadyFactoryPopped, PopNextReadyFactory, popNextReadyFactory)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import HList (HList (HNil))

type Setup :: Type -> [FactorySpec] -> Type
data Setup bundleMeta factorySpecs where
  Empty :: Setup bundleMeta '[]
  (:>>) ::
    ( Factory.ValidFactory spec
    , Factory.FactoryBundleMeta spec ~ bundleMeta
    ) =>
    Factory spec ->
    Setup bundleMeta specs ->
    Setup bundleMeta (spec ': specs)

infixr 5 :>>

assembleSetup ::
  forall bundleMeta specs assemblers.
  ( Assemble assemblers bundleMeta
  , BuildSetup bundleMeta specs
  ) =>
  assemblers ->
  HList (Factories specs) ->
  Assemble.AssembleResults assemblers
assembleSetup assemblers = Assemble.assemble assemblers . runSetup [] . buildSetup @bundleMeta

runSetup :: [DynamicBundle bundleMeta] -> Setup bundleMeta specs -> [DynamicBundle bundleMeta]
runSetup bundles setup = case setup of
  Empty -> bundles
  factory :>> more -> runSetup (Factory.addFromFactory factory bundles) more

-- BuildSetup takes an HList of Factories and builds a setup
-- I am not proud of this implementation

type BuildSetup :: Type -> [FactorySpec] -> Constraint
class
  ( Factory.ValidFactories specs
  , Factory.FactoriesHaveSameMeta bundleMeta specs
  ) =>
  BuildSetup bundleMeta specs
  where
  type BuildSetupResult specs :: [FactorySpec]
  buildSetup ::
    HList (Factories specs) ->
    Setup bundleMeta (BuildSetupResult specs)

instance BuildSetup bundleMeta '[] where
  type BuildSetupResult '[] = '[]
  buildSetup HNil = Empty

instance
  ( Factory.ValidFactories (spec_ : moreSpecs_)
  , Factory.FactoriesHaveSameMeta bundleMeta (spec_ ': moreSpecs_)
  , PopNextReadyFactory
      (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
      spec_
      moreSpecs_
  , (readySpec ':| nextSpecs)
      ~ NextReadyFactoryPopped
          (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
          spec_
          moreSpecs_
  , Factory.ValidFactories (readySpec ': nextSpecs)
  , Factory.FactoriesHaveSameMeta bundleMeta (readySpec ': nextSpecs)
  , BuildSetup bundleMeta nextSpecs
  ) =>
  BuildSetup bundleMeta (spec_ ': moreSpecs_)
  where
  type
    BuildSetupResult (spec_ ': moreSpecs_) =
      GoBuildSetupResult
        ( NextReadyFactoryPopped
            (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
            spec_
            moreSpecs_
        )
  buildSetup factories =
    case popNextReadyFactory
      @(Factory.AllFactoryOutputs (spec_ : moreSpecs_))
      factories of
      (nextFactory, moreFactories) -> nextFactory :>> buildSetup moreFactories

type family GoBuildSetupResult specs where
  GoBuildSetupResult (spec ':| moreSpecs) = spec : BuildSetupResult moreSpecs

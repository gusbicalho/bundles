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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
import Data.Functor ((<&>))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import HList (HList (HNil))

type Setup :: Type -> [FactorySpec] -> (Type -> Type) -> Type
data Setup bundleMeta factorySpecs m where
  Empty :: Setup bundleMeta '[] m
  (:>>) ::
    ( Factory.ValidFactory spec
    , Factory.FactoryBundleMeta spec ~ bundleMeta
    ) =>
    Factory m spec ->
    Setup bundleMeta specs m ->
    Setup bundleMeta (spec ': specs) m

infixr 5 :>>

assembleSetup ::
  forall bundleMeta specs assemblers.
  ( Assemble assemblers bundleMeta
  , BuildSetup bundleMeta specs IO
  , RunSetup (BuildSetupResult specs)
  ) =>
  assemblers ->
  HList (Factories IO specs) ->
  IO (Assemble.AssembleResults assemblers)
{-# INLINE assembleSetup #-}
assembleSetup assemblers factories =
  pure factories
    <&> buildSetup @bundleMeta
    >>= runSetup []
    >>= Assemble.assemble assemblers

-- RunSetup could have been a recursive function over the Setup GADT
-- but by using a class we can structurally recurse over the `specs` type-list
-- and get inlining!
class RunSetup specs where
  runSetup :: Monad m => [DynamicBundle bundleMeta] -> Setup bundleMeta specs m -> m [DynamicBundle bundleMeta]

instance RunSetup '[] where
  {-# INLINE runSetup #-}
  runSetup bundles Empty = pure bundles

instance (RunSetup moreSpecs) => RunSetup (spec ': moreSpecs) where
  {-# INLINE runSetup #-}
  runSetup bundles (factory :>> more) = do
    bundles <- Factory.addFromFactory factory bundles
    runSetup bundles more

-- BuildSetup takes an HList of Factories and builds a setup
-- I am not proud of this implementation

type BuildSetup :: Type -> [FactorySpec] -> (Type -> Type) -> Constraint
class
  ( Factory.ValidFactories specs
  , Factory.FactoriesHaveSameMeta bundleMeta specs
  ) =>
  BuildSetup bundleMeta specs m
  where
  type BuildSetupResult specs :: [FactorySpec]
  buildSetup ::
    HList (Factories m specs) ->
    Setup bundleMeta (BuildSetupResult specs) m

instance BuildSetup bundleMeta '[] m where
  type BuildSetupResult '[] = '[]
  {-# INLINE buildSetup #-}
  buildSetup HNil = Empty

instance
  ( Factory.ValidFactories (spec_ : moreSpecs_)
  , Factory.FactoriesHaveSameMeta bundleMeta (spec_ ': moreSpecs_)
  , PopNextReadyFactory
      (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
      spec_
      moreSpecs_
      m
  , (readySpec ':| nextSpecs)
      ~ NextReadyFactoryPopped
          (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
          spec_
          moreSpecs_
  , Factory.ValidFactories (readySpec ': nextSpecs)
  , Factory.FactoriesHaveSameMeta bundleMeta (readySpec ': nextSpecs)
  , BuildSetup bundleMeta nextSpecs m
  ) =>
  BuildSetup bundleMeta (spec_ ': moreSpecs_) m
  where
  type
    BuildSetupResult (spec_ ': moreSpecs_) =
      GoBuildSetupResult
        ( NextReadyFactoryPopped
            (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
            spec_
            moreSpecs_
        )
  {-# INLINE buildSetup #-}
  buildSetup factories =
    case popNextReadyFactory
      @(Factory.AllFactoryOutputs (spec_ : moreSpecs_))
      factories of
      (nextFactory, moreFactories) -> nextFactory :>> buildSetup moreFactories

type family GoBuildSetupResult specs where
  GoBuildSetupResult (spec ':| moreSpecs) = spec : BuildSetupResult moreSpecs

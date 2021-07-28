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
  assemblePureSetup,
) where

import Bundling.Assemble (Assemble)
import Bundling.Assemble qualified as Assemble
import Bundling.Bundle (DynamicBundle)
import Bundling.Factory (Factories, FactorySpec (..))
import Bundling.Factory qualified as Factory
import Bundling.Setup.PopNextReadyFactory (NextReadyFactoryPoppedSpecs, PopNextReadyFactory, popNextReadyFactory)
import Control.Applicative (liftA2)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Constraint, Type)
import HList (HList (HNil, (:::)))

type Setup :: Type -> [[FactorySpec]] -> (Type -> Type) -> Type
data Setup bundleMeta factorySpecs m where
  Empty :: Setup bundleMeta '[] m
  (:>>) ::
    ( Factory.ValidFactories newSpecs
    , Factory.FactoriesHaveSameMeta bundleMeta newSpecs
    ) =>
    HList (Factories m newSpecs) ->
    Setup bundleMeta specs m ->
    Setup bundleMeta (newSpecs : specs) m

infixr 5 :>>

assembleSetup ::
  forall bundleMeta specs assemblers m.
  ( Assemble m assemblers bundleMeta
  , BuildSetup bundleMeta specs m
  , RunSetup bundleMeta m (BuildSetupResult specs)
  , Monad m
  ) =>
  assemblers ->
  HList (Factories m specs) ->
  m (Assemble.AssembleResults assemblers)
{-# INLINE assembleSetup #-}
assembleSetup assemblers factories =
  pure factories
    <&> buildSetup @bundleMeta
    >>= runSetup []
    >>= Assemble.assemble assemblers

assemblePureSetup ::
  forall bundleMeta specs assemblers.
  ( Assemble Identity assemblers bundleMeta
  , BuildSetup bundleMeta specs Identity
  , RunSetup bundleMeta Identity (BuildSetupResult specs)
  ) =>
  assemblers ->
  HList (Factories Identity specs) ->
  Assemble.AssembleResults assemblers
{-# INLINE assemblePureSetup #-}
assemblePureSetup = (coerce .) . assembleSetup @bundleMeta @_ @_ @Identity

-- RunSetup could have been a recursive function over the Setup GADT
-- but by using a class we can structurally recurse over the `specs` type-list
-- and get inlining!
type RunSetup :: Type -> (Type -> Type) -> [[FactorySpec]] -> Constraint
class (Applicative m) => RunSetup bundleMeta m specs where
  runSetup :: [DynamicBundle bundleMeta] -> Setup bundleMeta specs m -> m [DynamicBundle bundleMeta]

instance Monad m => RunSetup bundleMeta m '[] where
  {-# INLINE runSetup #-}
  runSetup bundles Empty = pure bundles

instance
  ( Monad m
  , RunFactories bundleMeta m spec
  , RunSetup bundleMeta m moreSpecs
  ) =>
  RunSetup bundleMeta m (spec ': moreSpecs)
  where
  {-# INLINE runSetup #-}
  runSetup bundles (factories :>> more) = do
    newBundles <- runFactories @bundleMeta bundles factories
    runSetup (bundles ++ newBundles) more

class
  ( Applicative m
  , Factory.ValidFactories specs
  , Factory.FactoriesHaveSameMeta bundleMeta specs
  ) =>
  RunFactories bundleMeta m specs
  where
  runFactories :: [DynamicBundle bundleMeta] -> HList (Factories m specs) -> m [DynamicBundle bundleMeta]

instance Applicative m => RunFactories bundleMeta m '[] where
  runFactories _ HNil = pure []

instance
  ( Applicative m
  , Factory.ValidFactories (spec ': moreSpecs)
  , Factory.FactoriesHaveSameMeta bundleMeta (spec ': moreSpecs)
  , RunFactories bundleMeta m moreSpecs
  ) =>
  RunFactories bundleMeta m (spec ': moreSpecs)
  where
  runFactories bundles (factory ::: more) =
    liftA2
      (++)
      (Factory.runFactory factory bundles)
      (runFactories bundles more)

-- BuildSetup takes an HList of Factories and builds a setup
-- I am not proud of this implementation

type BuildSetup :: Type -> [FactorySpec] -> (Type -> Type) -> Constraint
class
  ( Factory.ValidFactories specs
  , Factory.FactoriesHaveSameMeta bundleMeta specs
  ) =>
  BuildSetup bundleMeta specs m
  where
  type BuildSetupResult specs :: [[FactorySpec]]
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
  , '(readySpecs, nextSpecs)
      ~ NextReadyFactoryPoppedSpecs
          (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
          spec_
          moreSpecs_
  , Factory.ValidFactories readySpecs
  , Factory.FactoriesHaveSameMeta bundleMeta readySpecs
  , BuildSetup bundleMeta nextSpecs m
  ) =>
  BuildSetup bundleMeta (spec_ ': moreSpecs_) m
  where
  type
    BuildSetupResult (spec_ ': moreSpecs_) =
      GoBuildSetupResult
        ( NextReadyFactoryPoppedSpecs
            (Factory.AllFactoryOutputs (spec_ : moreSpecs_))
            spec_
            moreSpecs_
        )
  {-# INLINE buildSetup #-}
  buildSetup factories =
    case popNextReadyFactory
      @(Factory.AllFactoryOutputs (spec_ : moreSpecs_))
      factories of
      (readyFactories, moreFactories) -> readyFactories :>> buildSetup moreFactories

type GoBuildSetupResult :: ([FactorySpec], [FactorySpec]) -> [[FactorySpec]]
type family GoBuildSetupResult specs where
  GoBuildSetupResult '(readySpecs, moreSpecs) =
   readySpecs ': BuildSetupResult moreSpecs

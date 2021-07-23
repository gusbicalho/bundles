{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Bundling.Setup (
  Setup (..),
  assembleSetup,
  runSetup,
) where

import Bundling.Assemble (Assemble)
import qualified Bundling.Assemble as Assemble
import Bundling.Bundle (DynamicBundle)
import Bundling.Factory (Factory, FactorySpec (..))
import qualified Bundling.Factory as Factory
import Data.Kind (Type)

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
  Assemble assemblers bundleMeta =>
  assemblers ->
  Setup bundleMeta specs ->
  Assemble.AssembleResults assemblers
assembleSetup assemblers = Assemble.assemble assemblers . runSetup []

runSetup :: [DynamicBundle bundleMeta] -> Setup bundleMeta specs -> [DynamicBundle bundleMeta]
runSetup bundles setup = case setup of
  Empty -> bundles
  factory :>> more -> runSetup (Factory.addFromFactory factory bundles) more

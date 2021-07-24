{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import qualified Bundling.TypeSet as TS
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import HList (HList)

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

-- ToSetup takes an HList of Factories and builds a setups

class ToSetup specs where
  toSetup :: HList (Factory.Factories specs) -> Setup
  -- TODO

type If :: forall k. Bool -> k -> k -> k
type family If c t e where
  If 'True t _ = t
  If 'False _ e = e

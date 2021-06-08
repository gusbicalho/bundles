{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Bundling.BundlingSetup (
  Bundles (..),
  Factories (..),
  BundlingSetup (..),
  emptySetup,
  addBundle,
  addFactory,
) where

import Bundling.Bundle (Bundle, BundleSig (..))
import Bundling.Factory (BundleFactory, BundleFactorySig (..))
import Data.Function ((&))
import Data.Kind (Type)

type Bundles :: [BundleSig] -> Type
data Bundles signatures where
  NoBundles :: Bundles '[]
  AddBundle ::
    Bundle name owner exports ->
    Bundles sigs ->
    Bundles ( 'BundleSig name owner exports : sigs)

type Factories :: [BundleFactorySig] -> Type
data Factories signatures where
  NoFactories :: Factories '[]
  AddFactory ::
    BundleFactory name owner imports exports ->
    Factories sigs ->
    Factories ( 'BundleFactorySig name owner imports exports : sigs)

type BundlingSetup :: [BundleSig] -> [BundleFactorySig] -> Type
data BundlingSetup bundles factories where
  BundlingSetup :: Bundles bundles -> Factories factories -> BundlingSetup bundles factories

emptySetup :: BundlingSetup '[] '[]
emptySetup = BundlingSetup NoBundles NoFactories

addBundle ::
  Bundle name owner exports ->
  BundlingSetup bundles factories ->
  BundlingSetup ( 'BundleSig name owner exports : bundles) factories
addBundle bundle (BundlingSetup bundles factories) =
  BundlingSetup (bundles & AddBundle bundle) factories

addFactory ::
  BundleFactory name owner imports exports ->
  BundlingSetup bundles factories ->
  BundlingSetup bundles ( 'BundleFactorySig name owner imports exports : factories)
addFactory factory (BundlingSetup bundles factories) =
  BundlingSetup bundles (factories & AddFactory factory)

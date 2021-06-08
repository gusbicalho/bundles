{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Bundling.Factory (
  BundleFactory (..),
  BundleFactorySig (..),
) where

import Bundling.Bundle (SomeBundle, TypeMap)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

type BundleFactory :: Symbol -> Symbol -> TypeMap -> TypeMap -> Type
data BundleFactory name owner imports exports where
  BundleFactory ::
    forall name owner imports exports.
    ([SomeBundle imports] -> [SomeBundle exports]) ->
    BundleFactory name owner imports exports

data BundleFactorySig where
  BundleFactorySig :: Symbol -> Symbol -> TypeMap -> TypeMap -> BundleFactorySig

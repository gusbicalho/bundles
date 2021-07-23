{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.Assemble (
  Assembler,
  runAssembler,
  assembler,
  assemble,
  foldMapper,
  collector,
  folder,
) where

import Bundling.Bundle (
  Bundle (..),
  DynamicBundle (..),
  ValidInputs,
  dynamicToTyped,
 )
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint, Type)
import Data.Maybe qualified as Maybe
import Data.Typeable (Typeable)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:)), TypeError)
import HList (HList (HNil, (:::)))

newtype Assembler bundleMeta t = Assembler {runAssembler :: [DynamicBundle bundleMeta] -> t}
  deriving stock (Functor)

type AssemblerBundleMeta :: Type -> Type
type family AssemblerBundleMeta spec where
  AssemblerBundleMeta (Assembler bundleMeta _) = bundleMeta
  AssemblerBundleMeta t =
    TypeError
      ( 'Text "AssemblerBundleMeta: Expected an Assembler, but found"
          ':$$: 'ShowType t
      )

assembler ::
  forall inputs output meta.
  ( ValidInputs inputs
  ) =>
  ([Bundle meta inputs] -> output) ->
  Assembler meta output
assembler doAssemble = Assembler (doAssemble . Maybe.mapMaybe (dynamicToTyped @inputs))

type Assemble :: Type -> Type -> Constraint
class Assemble assemblers bundleMeta where
  type AssembleResults assemblers :: Type
  assemble :: assemblers -> [DynamicBundle bundleMeta] -> AssembleResults assemblers

-- Useful assemblers

foldMapper ::
  forall u output meta.
  (Typeable u, Monoid output) =>
  (u -> output) ->
  Assembler meta output
foldMapper f = assembler $ Foldable.foldMap $ \(Bundle _ (e ::: HNil)) -> maybe mempty f e

folder :: forall t. (Typeable t, Monoid t) => Assembler String t
folder = foldMapper id

collector :: forall t. Typeable t => Assembler String [t]
collector = foldMapper (: [])

-- Assemble HList
instance Assemble (HList '[]) bundleMeta where
  type AssembleResults (HList '[]) = HList '[]
  assemble _ _ = HNil

type TypesOfHList :: Type -> [Type]
type family TypesOfHList hlist where
  TypesOfHList (HList ts) = ts
  TypesOfHList t =
    TypeError ( 'Text "Expected HList, but found " ':$$: 'ShowType t)

instance
  ( bundleMeta ~ assemblerBundleMeta
  , Assemble (HList moreAssemblers) bundleMeta
  , AssembleResults (HList moreAssemblers)
      ~ HList (TypesOfHList (AssembleResults (HList moreAssemblers)))
  ) =>
  Assemble (HList (Assembler assemblerBundleMeta assemblerResult ': moreAssemblers)) bundleMeta
  where
  type
    AssembleResults (HList (Assembler assemblerBundleMeta assemblerResult ': moreAssemblers)) =
      HList (assemblerResult ': TypesOfHList (AssembleResults (HList moreAssemblers)))
  assemble (asm ::: moreAsms) bundles =
    runAssembler asm bundles ::: assemble moreAsms bundles

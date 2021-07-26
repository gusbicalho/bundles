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
  Assemble (assemble),
  AssembleResults,
  foldMapper,
  collector,
  folder,
  assemblerPure,
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

newtype Assembler bundleMeta t = Assembler {runAssembler :: [DynamicBundle bundleMeta] -> IO t}
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
  ([Bundle meta inputs] -> IO output) ->
  Assembler meta output
{-# INLINE assembler #-}
assembler doAssemble = Assembler (doAssemble . Maybe.mapMaybe (dynamicToTyped @inputs))

assemblerPure ::
  forall inputs output meta.
  ( ValidInputs inputs
  ) =>
  ([Bundle meta inputs] -> output) ->
  Assembler meta output
assemblerPure doAssemble = assembler (pure . doAssemble)

type Assemble :: Type -> Type -> Constraint
class Assemble assemblers bundleMeta where
  type AssembleResults assemblers :: Type
  assemble :: assemblers -> [DynamicBundle bundleMeta] -> IO (AssembleResults assemblers)

-- Useful assemblers

foldMapper ::
  forall u output meta.
  (Typeable u, Monoid output) =>
  (u -> output) ->
  Assembler meta output
{-# INLINE foldMapper #-}
foldMapper f = assemblerPure $ Foldable.foldMap $ \(Bundle _ (e ::: HNil)) -> maybe mempty f e

folder :: forall t. (Typeable t, Monoid t) => Assembler String t
{-# INLINE folder #-}
folder = foldMapper id

collector :: forall t. Typeable t => Assembler String [t]
{-# INLINE collector #-}
collector = foldMapper pure

-- Assemble HList
instance Assemble (HList '[]) bundleMeta where
  type AssembleResults (HList '[]) = HList '[]
  {-# INLINE assemble #-}
  assemble _ _ = pure HNil

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
  {-# INLINE assemble #-}
  assemble (asm ::: moreAsms) bundles =
    (:::) <$> runAssembler asm bundles <*> assemble moreAsms bundles

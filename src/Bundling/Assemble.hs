{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Bundling.Assemble (
  Assembler,
  runAssembler,
  assembler,
  assemble,
) where

import Bundling.Bundle (
  Bundle (..),
  DynamicBundle (..),
  ValidInputs,
  dynamicToTyped,
 )
import Data.Kind (Constraint, Type)
import Data.Maybe qualified as Maybe
import Debug.Trace qualified as Trace
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:)), TypeError)
import HList (HList (HNil, (:::)))

newtype Assembler bundleMeta t = Assembler {runAssembler :: [DynamicBundle bundleMeta] -> t}

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
  , Show meta
  ) =>
  ([Bundle meta inputs] -> output) ->
  Assembler meta output
assembler doAssemble = Assembler (doAssemble . Maybe.mapMaybe dynamicToTyped . traceMetas)
 where
  traceMetas [] = []
  traceMetas (b@(DynamicBundle meta _) : more) = Trace.traceShow meta `seq` (b : traceMetas more)

type Assemble :: Type -> Type -> Constraint
class Assemble assemblers bundleMeta where
  type AssembleResults assemblers :: [Type]
  assemble :: assemblers -> [DynamicBundle bundleMeta] -> HList (AssembleResults assemblers)

instance Assemble (HList '[]) bundleMeta where
  type AssembleResults (HList '[]) = '[]
  assemble _ _ = HNil

instance
  ( bundleMeta ~ assemblerBundleMeta
  , Assemble (HList moreAssemblers) bundleMeta
  ) =>
  Assemble (HList (Assembler assemblerBundleMeta assemblerResult ': moreAssemblers)) bundleMeta
  where
  type
    AssembleResults (HList (Assembler assemblerBundleMeta assemblerResult ': moreAssemblers)) =
      assemblerResult ': AssembleResults (HList moreAssemblers)
  assemble (asm ::: moreAsms) bundles =
    runAssembler asm bundles ::: assemble moreAsms bundles

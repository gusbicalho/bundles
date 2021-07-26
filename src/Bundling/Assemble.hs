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
  assemblerPure,
  Assemble (assemble),
  assemblePure,
  AssembleResults,
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
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.Maybe qualified as Maybe
import Data.Typeable (Typeable)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:)), TypeError)
import HList (HList (HNil, (:::)))

newtype Assembler bundleMeta f t = Assembler {runAssembler :: [DynamicBundle bundleMeta] -> f t}
  deriving stock (Functor)

type AssemblerBundleMeta :: Type -> Type
type family AssemblerBundleMeta spec where
  AssemblerBundleMeta (Assembler bundleMeta _ _) = bundleMeta
  AssemblerBundleMeta t =
    TypeError
      ( 'Text "AssemblerBundleMeta: Expected an Assembler, but found"
          ':$$: 'ShowType t
      )

assembler ::
  forall inputs output meta f.
  ( ValidInputs inputs
  ) =>
  ([Bundle meta inputs] -> f output) ->
  Assembler meta f output
{-# INLINE assembler #-}
assembler doAssemble = Assembler (doAssemble . Maybe.mapMaybe (dynamicToTyped @inputs))

assemblerPure ::
  forall inputs output meta f.
  ( ValidInputs inputs
  , Applicative f
  ) =>
  ([Bundle meta inputs] -> output) ->
  Assembler meta f output
assemblerPure doAssemble = assembler (pure . doAssemble)

assemblePure ::
  Assemble Identity assemblers bundleMeta =>
  assemblers ->
  [DynamicBundle bundleMeta] ->
  AssembleResults assemblers
assemblePure assemblers = coerce . assemble @Identity assemblers

type Assemble :: (Type -> Type) -> Type -> Type -> Constraint
class (Applicative f) => Assemble f assemblers bundleMeta where
  type AssembleResults assemblers :: Type
  assemble :: assemblers -> [DynamicBundle bundleMeta] -> f (AssembleResults assemblers)

-- Useful assemblers

foldMapper ::
  forall u output meta f.
  ( Typeable u
  , Monoid output
  , Applicative f
  ) =>
  (u -> output) ->
  Assembler meta f output
{-# INLINE foldMapper #-}
foldMapper f = assemblerPure $ Foldable.foldMap $ \(Bundle _ (e ::: HNil)) -> maybe mempty f e

folder :: forall t f. (Typeable t, Monoid t, Applicative f) => Assembler String f t
{-# INLINE folder #-}
folder = foldMapper id

collector :: forall t f. (Typeable t, Applicative f) => Assembler String f [t]
{-# INLINE collector #-}
collector = foldMapper pure

-- Assemble HList
instance (Applicative f) => Assemble f (HList '[]) bundleMeta where
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
  , Assemble f (HList moreAssemblers) bundleMeta
  , AssembleResults (HList moreAssemblers)
      ~ HList (TypesOfHList (AssembleResults (HList moreAssemblers)))
  , f ~ assembler_f
  ) =>
  Assemble f (HList (Assembler assemblerBundleMeta assembler_f assemblerResult ': moreAssemblers)) bundleMeta
  where
  type
    AssembleResults (HList (Assembler assemblerBundleMeta assembler_f assemblerResult ': moreAssemblers)) =
      HList (assemblerResult ': TypesOfHList (AssembleResults (HList moreAssemblers)))
  {-# INLINE assemble #-}
  assemble (asm ::: moreAsms) bundles =
    (:::) <$> runAssembler asm bundles <*> assemble moreAsms bundles

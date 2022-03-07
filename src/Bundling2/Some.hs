{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Bundling2.Some (Some (..), withSome, onSome, Is, And, All, Unconstrained) where

import Data.Kind (Constraint, Type)

-- Existential

type Some :: (Type -> Constraint) -> Type
data Some c where
  MkSome :: c t => !t -> Some c

withSome :: Some c -> (forall a. c a => a -> r) -> r
withSome (MkSome a) f = f a

onSome :: (forall a. c a => a -> r) -> Some c -> r
onSome f a = withSome a f

-- Useful instances for Some

showsPrecSome ::
  forall c.
  (forall a. c a => Show a) =>
  Int ->
  Some c ->
  ShowS
showsPrecSome d (MkSome a) =
  showParen (d > app_prec) $
    showString "MkSome " . showsPrec (app_prec + 1) a
 where
  app_prec = 10

instance
  (forall a. c a => Show a) =>
  Show (Some c)
  where
  showsPrec = showsPrecSome
  show some = showsPrecSome 0 some ""
  showList somes = showList' (showsPrecSome 0 <$> somes)
   where
    showList' [] s = "[]" <> s
    showList' [showS] s = showS $ "]" <> s
    showList' (showS : moreShows) s = showS $ showList' moreShows s

-- Recover concreteness in existentials 😬

class (a ~ b) => Is a b
instance (a ~ b) => Is a b

-- Aggregate constraints

class (c1 a, c2 a) => And c1 c2 a
instance (c1 a, c2 a) => And c1 c2 a

class Unconstrained a
instance Unconstrained a

type All :: [Type -> Constraint] -> Type -> Constraint
type family All cs where
  All '[] = Unconstrained
  All (c ': moreCs) = And c (All moreCs)

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Bundling2.Some (Some (..), Is) where

import Data.Kind (Constraint, Type)

-- Existential

type Some :: (Type -> Constraint) -> Type
data Some c where
  MkSome :: c t => !t -> Some c

class (a ~ b) => Is a b
instance (a ~ b) => Is a b

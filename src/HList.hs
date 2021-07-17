{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HList (HList (..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)

-- definition copied from HList package

type HList :: [Type] -> Type
data family HList ts

data instance HList '[] = HNil
  deriving stock (Generic)

data instance HList (x ': xs) = x ::: HList xs
  deriving stock (Generic)

infixr 5 :::

deriving stock instance Eq (HList '[])

deriving stock instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving stock instance Ord (HList '[])

deriving stock instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))

deriving stock instance Show (HList '[])
deriving stock instance (Show x, Show (HList xs)) => Show (HList (x ': xs))

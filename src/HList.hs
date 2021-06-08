{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HList (HList (..)) where

import Data.Kind (Type)

-- definition copied from HList package

type HList :: [Type] -> Type
data family HList ts

data instance HList '[] = HNil

data instance HList (x ': xs) = x ::: HList xs

infixr 5 :::

deriving stock instance Eq (HList '[])

deriving stock instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving stock instance Ord (HList '[])

deriving stock instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))

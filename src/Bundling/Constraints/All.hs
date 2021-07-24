{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Bundling.Constraints.All (All, AllF) where

import Data.Kind (Constraint, Type)

type AllF :: [Type -> Constraint] -> Type -> Constraint
type family AllF cs t where
  AllF '[] t = ()
  AllF (c : cs) t = (c t, AllF cs t)

type All :: [Type -> Constraint] -> Type -> Constraint
class (AllF cs t) => All cs t
instance (AllF cs t) => All cs t

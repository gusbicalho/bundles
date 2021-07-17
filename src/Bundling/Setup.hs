{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Bundling.Setup where

import Bundling.Factory (FactorySpec (..), Factory)
import Data.Kind (Type)

type Setup :: [FactorySpec] -> Type
data Setup factorySpecs where
  Empty :: Setup '[]
  (:>>) :: Factory spec -> Setup specs -> Setup (spec ': specs)

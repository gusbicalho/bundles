{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Scratch () where

-- type GetExports :: TypeMap -> [Type -> Constraint]
-- type family GetExports exs where
--   GetExports '[] = '[]
--   GetExports (exportName ':-> export : exs) = (GetExport exportName export : GetExports exs)

-- type GetExports :: TypeMap -> [Type -> Constraint]
-- type family GetExports exs where
--   GetExports '[] = '[]
--   GetExports (exportName ':-> export : exs) = (GetExport exportName export : GetExports exs)
import Data.Kind (Constraint, Type)

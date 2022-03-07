{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Bundling2.DynBag (
  DynBag (..),
  get,
  getList,
  empty,
  singleton,
  fromDynamics,
  extend,
  restrict,
  Some,
  Dynamic,
  toDyn,
  ToTypeReps (..),
) where

import Bundling2.Some (Some (MkSome), onSome)
import Data.Dynamic (Dynamic (Dynamic), toDyn)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import GHC.Base (Any)
import Type.Reflection (SomeTypeRep (SomeTypeRep), someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

class DynBag bag where
  foldMapItems ::
    forall
      (a :: Type)
      (m :: Type)
      (proxy :: Type -> Type).
    (Typeable a, Monoid m) =>
    proxy a ->
    (a -> m) ->
    bag ->
    m

get :: forall a t. (Typeable a, Monoid (t a), Applicative t) => Some DynBag -> t a
get = onSome $ foldMapItems (Proxy @a) pure

getList :: forall a. (Typeable a) => Some DynBag -> [a]
getList = get @a @[]

---

newtype MapBag = MkMapBag (Map SomeTypeRep [Any])

instance DynBag MapBag where
  foldMapItems p toMonoid (MkMapBag m) =
    case Map.lookup (typeRep p) m of
      Nothing -> mempty
      Just anys -> foldMap (toMonoid . unsafeCoerce) anys

empty :: Some DynBag
empty = MkSome $ MkMapBag Map.empty

singleton :: forall a. Typeable a => a -> Some DynBag
singleton a = MkSome $ MkMapBag $ Map.singleton (typeRep $ Proxy @a) (unsafeCoerce a)

fromDynamics :: [Dynamic] -> Some DynBag
fromDynamics = MkSome . MkMapBag . Map.map reverse . foldl' add Map.empty
 where
  add m dyn = case dyn of
    Dynamic tr a ->
      Map.insertWith
        (<>)
        (SomeTypeRep tr)
        [unsafeCoerce a]
        m

---

data ExtendBag where
  MkExtendBag ::
    forall bag1 bag2.
    (DynBag bag1, DynBag bag2) =>
    bag1 ->
    bag2 ->
    ExtendBag

instance DynBag ExtendBag where
  foldMapItems p toMonoid (MkExtendBag bag1 bag2) =
    foldMapItems p toMonoid bag1 <> foldMapItems p toMonoid bag2

extend :: Some DynBag -> Some DynBag -> Some DynBag
extend (MkSome bag1) (MkSome bag2) = MkSome $ MkExtendBag bag1 bag2

---

data RestrictBag where
  MkRestrictBag :: forall bag. DynBag bag => bag -> Set SomeTypeRep -> RestrictBag

instance DynBag RestrictBag where
  foldMapItems p toMonoid (MkRestrictBag bag exportedTypes)
    | typeRep p `Set.member` exportedTypes = foldMapItems p toMonoid bag
    | otherwise = mempty

restrict :: Set SomeTypeRep -> Some DynBag -> Some DynBag
restrict exportedTypes (MkSome bag) = MkSome $ MkRestrictBag bag exportedTypes

-- TypeReps helper

class ToTypeReps (types :: [Type]) where
  typeReps :: Set SomeTypeRep

instance ToTypeReps '[] where
  typeReps = Set.empty

instance (Typeable t, ToTypeReps more) => ToTypeReps (t ': more) where
  typeReps = Set.insert (someTypeRep (Proxy @t)) (typeReps @more)

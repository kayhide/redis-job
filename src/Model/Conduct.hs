{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
module Model.Conduct
  ( Required (..)
  , Optional (..)
  , educt
  , conduct
  , induct
  , induct'
  )
where

import ClassyPrelude

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Extensible
import Data.Type.Bool (If)


newtype Required a = Required a
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Default)

newtype Optional a = Optional (Maybe a)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Default)


-- | Lift inner functor of each field.
-- Functor @f@ is transoformed to @g@ by @Transform f g@ instance.
-- Output type should be given by a function caller.

educt
  :: forall g xs ys.
     Forall (TransformField g xs) ys
  => Record xs
  -> RecordOf g ys
educt r =
  htabulateFor (Proxy @(TransformField g xs)) $ \m -> Field $ elemF m r


-- | Specialized version of @educt@ where @f@ = @Maybe@.
-- @xs@ are mapped while de-functorized.
-- Thus, output type is infered by itself.

conduct
  :: ( g ~ Maybe
     , ys ~ DefunctorAssoc xs
     , Forall (TransformField g xs) ys
     )
  => Record xs
  -> RecordOf g ys
conduct = educt


-- | Update a record with given fields.
-- Updating fields should be a subset of the updating record.
-- If the fields do not have inclusion constraint, consider @induct@ instead.

induct' :: (Generate ys, Include ys xs)
  => RecordOf Maybe xs
  -> Record ys
  -> Record ys
induct' xs ys = hzipWith f ys $ wrench xs
  where
    f :: Field Identity a -> Nullable (Field Maybe) a -> Field Identity a
    f y x = case getField <$> getNullable x of
      (Just (Just x')) -> Field $ Identity x'
      _                -> y


-- | Update a record with given fields.
-- Updating fields is not necessarily a subset of the record.

induct
  :: forall xs ys zs.
     ( Generate ys
     , zs ~ Intersection xs ys
     , Include xs zs
     , Include ys zs
     )
  => RecordOf Maybe xs
  -> Record ys
  -> Record ys
induct xs ys = hzipWith f ys $ shrench xs
  where
    f :: Field Identity a -> Nullable (Field Maybe) a -> Field Identity a
    f y x = case getField <$> getNullable x of
      (Just (Just x')) -> Field $ Identity x'
      _                -> y

shrench
  :: forall h xs ys zs.
     ( Generate ys
     , zs ~ Intersection xs ys
     , Include xs zs
     , Include ys zs
     )
  => h :* xs -> Nullable h :* ys
shrench xs = wrench $ (shrink xs :: h :* zs)
{-# INLINE shrench #-}




-- * Internal

-- | Transform type class defines type-specific natural transformation.

class Transform f g where
  trans :: f x -> g x

instance Transform a a where
  trans = id

instance Transform Required (Nullable Required) where
  trans x = Nullable $ Just x

instance Transform Optional (Nullable Required) where
  trans _ = Nullable Nothing

instance Transform Required (Nullable Optional) where
  trans _ = Nullable Nothing

instance Transform Optional (Nullable Optional) where
  trans x = Nullable $ Just x

instance Transform Required Maybe where
  trans (Required x) = Just x

instance Transform Optional Maybe where
  trans (Optional x) = x


-- | TransformField type class is used by @educt@.

class TransformField g xs kv where
  elemF :: proxy kv -> Record xs -> g (AssocValue kv)


-- | TransformField instance.
-- Since @educt@ is implemented where constraint is required, this cannot be a
-- top-level function even though the instance is fully parametarized.

instance
  ( Transform f g
  , Associate k (f v) xs
  , k ~ AssocKey kv
  , v ~ AssocValue kv
  )
  => TransformField g xs kv where
  elemF _ r = trans $ r ^. itemAssoc (Proxy @k)


-- | DefunctorAssoc type family
-- Type level defunctorizer which works on a list of @Assoc@s with functor values.

type family DefunctorAssoc (kvs :: [Assoc k v]) :: [Assoc k *]
type instance DefunctorAssoc '[] = '[]
type instance DefunctorAssoc ((k >: f x) ': kvs) = (k >: x) ': DefunctorAssoc kvs


-- * Type list operations
-- Borrowed from [type-list](http://hackage.haskell.org/package/type-list) package.

-- | Type list membership test.
type family Find x ys where
    Find x '[]       = 'False
    Find x (x ': ys) = 'True
    Find x (y ': ys) = Find x ys

-- | Type list intersection.
type family Intersection xs ys where
    Intersection '[] ys = '[]
    Intersection (x ': xs) (x ': ys) = x ': (Intersection xs ys)
    Intersection (x ': xs) (y ': ys) = If (Find x ys) (x ': (Intersection xs (y ': ys))) (Intersection xs (y ': ys))

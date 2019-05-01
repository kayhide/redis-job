{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
module Conduct
  ( educt
  , conduct
  , induct
  -- * Testing
  , main
  )
where

import ClassyPrelude

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Extensible


main :: IO ()
main = do
  putStrLn "Reqdy..."
  sayShow $ (educt alice :: PersonRequiredParams)
  sayShow $ (educt alice :: PersonOptionalParams)
  sayShow $ (educt alice :: PersonNullableRequiredParams)
  sayShow $ (educt alice :: PersonNullableOptionalParams)
  sayShow $ (educt bob :: PersonRequiredParams)
  sayShow $ (educt bob :: PersonOptionalParams)
  sayShow $ (educt bob :: PersonNullableRequiredParams)
  sayShow $ (educt bob :: PersonNullableOptionalParams)
  sayShow $ (educt alice :: PersonMaybeParams)
  sayShow $ (educt alice :: PersonMaybeParams)
  sayShow $ conduct alice
  sayShow $ conduct bob
  sayShow girl
  sayShow $ induct girl $ conduct alice

girl :: Person
girl
  = #id @= 8
  <: #name @= "betty"
  <: #age  @= 3
  <: nil


alice :: PersonParams
alice
   = #name @= Required "alice"
  <: #age  @= Optional (Just 21)
  <: nil

bob :: PersonParams
bob
   = #name @= Required "bob"
  <: #age  @= Optional Nothing
  <: nil

newtype Required a = Required a
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Default)

newtype Optional a = Optional (Maybe a)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Default)

instance Wrapper Required where
  type Repr Required x = x
  wrap = Required
  unwrap (Required x) = x

instance Wrapper Optional where
  type Repr Optional x = Maybe x
  wrap = Optional
  unwrap (Optional x) = x


type Person =
  Record
  '[ "id" >: Int
   , "name" >: Text
   , "age" >: Int
   ]

type PersonParams =
  Record
  '[ "name" >: Required Text
   , "age" >: Optional Int
   ]

type PersonRequiredParams =
  RecordOf Required
  '[ "name" >: Text
   ]

type PersonOptionalParams =
  RecordOf Optional
  '[ "age" >: Int
   ]

type PersonNullableRequiredParams =
  RecordOf (Nullable Required)
  '[ "name" >: Text
   , "age" >: Int
   ]

type PersonNullableOptionalParams =
  RecordOf (Nullable Optional)
  '[ "name" >: Text
   , "age" >: Int
   ]

type PersonMaybeParams =
  RecordOf Maybe
  '[ "name" >: Text
   , "age" >: Int
   ]


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


-- | Specialized version of @educt@ where @f@ = @Maybe@.
-- @xs@ are mapped while de-functorized.
-- Thus, output type is infered by itself.

induct :: (Generate xs, Include xs ys) => Record xs -> (Field Maybe) :* ys -> Record xs
induct xs ys = hzipWith f xs $ wrench ys
  where
    f :: Field Identity a -> Nullable (Field Maybe) a -> Field Identity a
    f x y = case getField <$> getNullable y of
      (Just (Just y')) -> Field $ Identity y'
      _                -> x


-- * Transform type class and instances

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


-- * TransformField type class and instance
-- This is used by @educt@.
-- Since constraint is required, this needs to be a type class, even though
-- the instance is fully parametarized.

class TransformField g xs kv where
  elemF :: proxy kv -> Record xs -> g (AssocValue kv)

instance
  ( Transform f g
  , Associate k (f v) xs
  , k ~ AssocKey kv
  , v ~ AssocValue kv
  )
  => TransformField g xs kv where
  elemF _ r = trans $ r ^. itemAssoc (Proxy @k)


-- * DefunctorAssoc type family
-- Type level defunctorizer which works on a list of @Assoc@s with functor values.

type family DefunctorAssoc (kvs :: [Assoc k v]) :: [Assoc k *]
type instance DefunctorAssoc '[] = '[]
type instance DefunctorAssoc ((k >: f x) ': kvs) = (k >: x) ': DefunctorAssoc kvs

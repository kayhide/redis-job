{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
module Conduct where

import ClassyPrelude

import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Extensible

main :: IO ()
main = do
  sayShow $ (educt alice :: PersonRequiredParams)
  sayShow $ (educt alice :: PersonOptionalParams)
  sayShow $ (conduct alice :: PersonNullableRequiredParams)
  sayShow $ (conduct alice :: PersonNullableOptionalParams)
  sayShow $ (educt bob :: PersonRequiredParams)
  sayShow $ (educt bob :: PersonOptionalParams)
  sayShow $ (conduct bob :: PersonNullableRequiredParams)
  sayShow $ (conduct bob :: PersonNullableOptionalParams)
  -- sayShow $ conduct @Required alice


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


-- | Educt inter functor lifting out.
-- This requires result type which consists of only fields with given functor type.

class ElemF xs f kv where
  elemF :: proxy kv -> Record xs -> f (AssocValue kv)

instance Associate (AssocKey kv) (f (AssocValue kv)) xs => ElemF xs f kv where
  elemF _ r = r ^. itemAssoc (Proxy @(AssocKey kv))

educt :: forall f xs ys. Forall (ElemF xs f) ys => Record xs -> RecordOf f ys
educt r =
  htabulateFor (Proxy @(ElemF xs f)) $ \m -> Field $ elemF m r


-- | Conduct inner functor lifting out.
-- This keeps original fields trying to lift as given functor type.
-- Fields with unmatched to the functor will be nullified.
--
-- TODO How can we separate out unmatched fields automatically?

class NullableElemF xs f kv where
  elemFMay :: proxy kv -> Record xs -> Nullable f (AssocValue kv)

-- | The following instances are dispathcing by hand.

instance NullableElemF '["name" >: Required Text, "age" >: Optional Int] Required ("name" >: Text) where
  elemFMay _ r = Nullable $ r ^? #name

instance NullableElemF '["name" >: Required Text, "age" >: Optional Int] Required ("age" >: Int) where
  elemFMay _ r = Nullable Nothing

instance NullableElemF '["name" >: Required Text, "age" >: Optional Int] Optional ("name" >: Text) where
  elemFMay _ r = Nullable Nothing

instance NullableElemF '["name" >: Required Text, "age" >: Optional Int] Optional ("age" >: Int) where
  elemFMay _ r = Nullable $ r ^? #age
  -- elemFMay _ r = shrinked' ^. #age
  -- elemFMay _ r = _
  --   where
  --     educted' :: (Field Optional) :* '["name" >: Text, "age" >: Int]
  --     educted' = educt $ shrinked'
  --     shrinked' :: Nullable (Field Identity) :* '["name" >: Optional Text, "age" >: Optional Int]
  --     shrinked' = shrink $ wrenched'
  --     wrenched' :: Nullable (Field Identity) :* (Union '["name" >: Required Text, "age" >: Optional Int] '["name" >: Optional Text, "age" >: Optional Int])
  --     wrenched' = wrench r

-- |Insert a type into a type list.
type family Insert a xs where
    Insert a '[]       = (a ': '[])
    Insert a (a ': xs) = (a ': xs)
    Insert a (x ': xs) = x ': (Insert a xs)


-- |Set union over type lists.
type family Union xs ys where
    Union '[] ys = ys
    Union (x ': xs) ys = Insert x (Union xs ys)

conduct :: forall f xs ys. Forall (NullableElemF xs f) ys => Record xs -> RecordOf (Nullable f) ys
conduct r =
  htabulateFor (Proxy @(NullableElemF xs f)) $ \m -> Field $ elemFMay m r


-- | This instance catches all cases and type error happens when `f` is not consistent.

-- instance ( Associate (AssocKey kv) (f (AssocValue kv)) xs
--          ) => NullableElemF xs f kv where
--   -- elemFMay _ r = Nullable $ r ^? itemAssoc (Proxy @(AssocKey kv))
--   elemFMay _ r = Nullable Nothing


-- | The following is field lookup approach, it calls `FindAssoc` and
-- dispatchs by its result with type class.
-- Could not find out how to resolve the ambiguity of `pos`.

-- instance ( LookupAssoc k pos (Record xs) (f v)
--          , pos ~ (FindAssoc 0 k xs)
--          , k ~ (AssocKey kv)
--          , v ~ (AssocValue kv)
--          ) => NullableElemF xs f kv where
--   elemFMay _ r = Nullable $ lookupAssoc @k @pos @_ @(f v) (Proxy @k) r


-- class LookupAssoc (k :: *) (pos :: [Assoc Nat v]) r a where
--   lookupAssoc :: proxy k -> r -> Maybe a

-- instance LookupAssoc k '[] r a where
--   lookupAssoc _ r = Nothing

-- instance (Associate k v xs) => LookupAssoc k '[n :> v] (Record xs) v where
--   lookupAssoc _ r = r ^? itemAt (association @k @v @xs)

-- instance LookupAssoc k (nv ': nvs) r a where
--   lookupAssoc _ r = Nothing



-- | The following is trying to overlap on "valid" cases.
-- This does not work because when instance is chosen, the costraints are already chosen.

-- instance {-# OVERLAPS #-} (Associate (AssocKey kv) (Required (AssocValue kv)) xs) => NullableElemF xs Required kv where
--   elemFMay _ r = Nullable $ r ^? itemAssoc (Proxy @(AssocKey kv))
-- instance {-# OVERLAPS #-} (Associate (AssocKey kv) (Optional (AssocValue kv)) xs) => NullableElemF xs Optional kv where
--   elemFMay _ r = Nullable $ r ^? itemAssoc (Proxy @(AssocKey kv))

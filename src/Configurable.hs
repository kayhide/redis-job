{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Configurable where

import           ClassyPrelude

import GHC.Prim(unsafeCoerce#)
import           Control.Lens          (Lens', lens, _1, _2)
import           Data.Extensible       (type (++), type (:*), Forall, Member (..), item, nil, shrink, hlength, hlookup)
import           Data.Extensible.Plain (AllOf, pluck, (<%))
import           Data.Extensible.Class(remember, Membership)
import           Data.Kind             (Constraint)
import           Data.Proxy            (Proxy (..))
import           System.Environment    (lookupEnv)
import Data.Extensible.Struct(hfrozen, toHList, newFromHList)
import qualified Data.Extensible.HList as L


class Configurable (a :: *) where
  type Setting a :: *
  type Running a :: *
  type Deps a :: [*]

  ready :: f a -> IO (Setting a)

  start
    :: (Forall Configurable (Deps a), Forall (HasConfig env) (Deps a))
    => f a 
    -> Setting a
    -> env
    -> IO (Running a)

  activate
    :: (Forall Configurable (Deps a), Forall (HasConfig env) (Deps a))
    => f a
    -> env
    -> IO (Setting a, Running a)
  activate proxy env = do
    setting' <- ready proxy
    running' <- start proxy setting' env
    pure (setting', running')

class Configurable a => HasConfig env a where
  setting :: f a -> Lens' env (Setting a)
  running :: f a -> Lens' env (Running a)

instance ( Configurable a
         , Member settings (Setting a)
         , Member runnings (Running a)
         ) => HasConfig (AllOf settings, AllOf runnings) a where
  setting _ = _1 . item (Proxy @(Setting a))
  running _ = _2 . item (Proxy @(Running a))


class FetchSetting a where
  fetchSetting :: Text -> a -> IO a
  default fetchSetting :: (Read a) => Text -> a -> IO a
  fetchSetting key def =
    fromMaybe def . (readMay =<<) <$> lookupEnv (unpack key)

instance FetchSetting Int

instance FetchSetting Text where
  fetchSetting key def =
    maybe def pack <$> lookupEnv (unpack key)


instance Configurable (AllOf '[]) where
  type Setting (AllOf '[]) = AllOf '[]
  type Running (AllOf '[]) = AllOf '[]
  type Deps (AllOf '[]) = '[]

  ready _ = pure nil
  start _ _ _ = pure nil

instance HasConfig env (AllOf '[]) where
  setting _ = lens (const nil) const
  running _ = lens (const nil) const

instance (HasConfig env a, HasConfig env (AllOf as)) => HasConfig env (AllOf (a ': as)) where
  -- setting _ = lens _ (setting (Proxy @a) <*> setting (Proxy (AllOf as))

-- Setting AllOf ([a,b,c])
-- ConsAllOf (Setting a) (Setting (AllOf [b,c]))
-- ConsAllOf (Setting a) (ConsAllOf (Setting b) (Setting (AllOf [:*c])))
-- ConsAllOf (Setting a) (ConsAllOf (Setting b) (ConsAllOf (Setting c) (Setting (AllOf [])))
-- ConsAllOf (Setting a) (ConsAllOf (Setting b) (ConsAllOf (Setting c) (Setting (Idenity :* [])))
-- ConsAllOf (Setting a) (ConsAllOf (Setting b) (ConsAllOf (Setting c) []))


type family ConsAllOf (x :: *) (xs :: *) = (r :: *) | r -> x xs where
  ConsAllOf x (h :* xs) = h :* (x ': xs)


type family Id x where
    Id x = x


data Dict p where
    Dict :: p => Dict p


{-
settingsProof :: f a -> f' (AllOf as) -> Dict (Identity :* ys ~ Setting (AllOf as),
    Setting (Identity :* (a : as)) ~ AllOf (Setting a : ys)
    )
settingsProof a as = 
    case as of
        Identity :* []
-}

{-
instance (Configurable a)
    => Configurable (AllOf '[a]) where
   type Setting (AllOf '[a]) = ConsAllOf (Setting a) (Setting (AllOf '[]))
   type Running (AllOf '[a]) = ConsAllOf (Running a) (Running (AllOf '[]))
   type Deps (AllOf '[a]) = Deps a 

   ready _ = do
     setting' <- ready (Proxy @a)
     pure $ setting' <% nil

   start _ settings env = do
     running' <- start (Proxy @a) (pluck settings) env
     runnings' <- start (Proxy @(AllOf '[])) nil env
     pure $ running' <% runnings'
-}

instance (Configurable a, Configurable (AllOf as))
    => Configurable (AllOf (a ': as)) where
   type Setting (AllOf (a ': as)) = ConsAllOf (Setting a) (Setting (AllOf as))
   type Running (AllOf (a ': as)) = ConsAllOf (Running a) (Running (AllOf as))
   type Deps (AllOf (a ': as)) = Deps a ++ Deps (AllOf as)

{-
   ready _ = do
     setting' <- ready (Proxy @a)
     pure $ setting' <% nil
-}
   start _ settings env = undefined
     -- case toHList settings of
     --      L.HCons (Identity x) xs -> do
     --        running' <- start (Proxy @a) x env
     --        undefined
         -- runnings' <- start (Proxy @(AllOf as)) (fromHList xs) env
         -- pure $ running' <% runnings'
     --
     --
--
-- | Convert 'HList.HList' into a product.
fromHList :: L.HList h xs -> h :* xs
fromHList xs = hfrozen (newFromHList xs)
{-# INLINE fromHList #-}

{-
instance (Configurable a, Configurable (Identity :* as))
    => Configurable (Identity :* (a ': as)) where
   type Setting (Identity :* (a ': as)) = ConsAllOf (Setting a) (Setting (AllOf as))
   type Running (AllOf (a ': as)) = ConsAllOf (Running a) (Running (AllOf as))
   type Deps (AllOf (a ': as)) = Deps a ++ Deps (AllOf as)

   ready _ = do
     setting' <- ready (Proxy @a)
     settings' <- ready (Proxy @(AllOf as))
     pure $ setting' <% settings'
-}
 
 {-
   start _ settings env = do
     running' <- start (pluck settings :: Setting a) env
     runnings' <- start nil env
     pure $ running' <% runnings'
-}



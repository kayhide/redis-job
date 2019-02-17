{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Configurable where

import           ClassyPrelude

import           Control.Lens          (Lens', lens, _1, _2)
import           Data.Extensible       (type (++), type (:*), Forall, Member (..), item, nil)
import           Data.Extensible.Plain (AllOf, pluck, (<%))
import           Data.Kind             (Constraint)
import           Data.Proxy            (Proxy (..))
import           System.Environment    (lookupEnv)


class Configurable (a :: *) where
  type Setting a = r | r -> a
  type Running a = r | r -> a
  type Deps a :: [*]

  ready :: IO (Setting a)

  start
    :: (Forall Configurable (Deps a), Forall (HasConfig env) (Deps a))
    => Setting a
    -> env
    -> IO (Running a)

  activate
    :: (Forall Configurable (Deps a), Forall (HasConfig env) (Deps a))
    => env
    -> IO (Setting a, Running a)
  activate env = do
    setting' <- ready
    running' <- start setting' env
    pure (setting', running')


class Configurable a => HasConfig env a where
  setting :: Lens' env (Setting a)
  running :: Lens' env (Running a)


instance ( Configurable a
         , Member settings (Setting a)
         , Member runnings (Running a)
         ) => HasConfig (AllOf settings, AllOf runnings) a where
  setting = _1 . item (Proxy @(Setting a))
  running = _2 . item (Proxy @(Running a))


class FetchSetting a where
  fetchSetting :: Text -> a -> IO a
  default fetchSetting :: (Read a) => Text -> a -> IO a
  fetchSetting key def =
    fromMaybe def . (readMay =<<) <$> lookupEnv (unpack key)

instance FetchSetting Int

instance FetchSetting Text where
  fetchSetting key def =
    maybe def pack <$> lookupEnv (unpack key)


-- type family KMap (f :: k -> k) (xs :: [k]) :: [k] where
--   KMap f '[]       = '[]
--   KMap f (x ': xs) = f x ': KMap f xs

type family ConsAllOf x xs where
  ConsAllOf x (h :* xs) = h :* (x ': xs)

instance Configurable (AllOf '[]) where
  type Setting (AllOf '[]) = AllOf '[]
  type Running (AllOf '[]) = AllOf '[]
  type Deps (AllOf '[]) = '[]

  ready = pure nil
  start _ _ = pure nil

instance HasConfig env (AllOf '[]) where
  setting = lens (const nil) const
  running = lens (const nil) const

-- instance (Configurable a, Configurable (AllOf as)) => Configurable (AllOf (a ': as)) where
--   type Setting (AllOf (a ': as)) = ConsAllOf (Setting a) (Setting (AllOf as))
--   -- type Setting (AllOf (a ': as)) = AllOf (Setting a ': as)
--   -- type Setting (AllOf (a ': as)) = AllOf (Setting a ': (Setting (AllOf as)))
--   -- type Running (AllOf (a ': as)) = AllOf (Running a ': (KMap Running as))
--   type Running (AllOf (a ': as)) = AllOf (Running a ': as)
--   type Deps (AllOf (a ': as)) = Deps a ++ Deps (AllOf as)

instance (Configurable a) => Configurable (AllOf '[a]) where
  type Setting (AllOf '[a]) = AllOf '[Setting a]
  type Running (AllOf '[a]) = AllOf '[Running a]
  type Deps (AllOf (a ': '[])) = '[] ++ Deps a

  ready = do
    setting' <- ready
    settings' <- ready
    pure $ setting' <% settings'

  start settings env = do
    running' <- start (pluck settings :: Setting a) env
    runnings' <- start nil env
    pure $ running' <% runnings'

-- instance (Configurable a, Configurable b) => Configurable (AllOf '[a, b]) where
--   type Setting (AllOf '[a, b]) = AllOf '[Setting a, Setting b]
--   type Running (AllOf '[a, b]) = AllOf '[Running a, Running b]
--   type Deps (AllOf (a ': '[b])) = Deps a ++ Deps b

--   ready = do
--     setting' <- ready
--     setting'' <- ready
--     settings' <- ready
--     pure $ setting' <% setting'' <% settings'

--   start settings env = do
--     running' <- start (pluck settings :: Setting a) env
--     running'' <- start (pluck settings :: Setting b) env
--     runnings' <- start nil env
--     pure $ running' <% running'' <% runnings'

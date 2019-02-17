{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
module Configurable where

import           ClassyPrelude

import           Control.Lens       (Lens')
import           Data.Kind          (Constraint)
import           GHC.TypeLits       (Symbol)
import           System.Environment (lookupEnv)


class Configurable (a :: *) where
  type Name a = (r :: Symbol) | r -> a
  type Setting a = r | r -> a
  type Running a = r | r -> a
  type Deps a :: [*]

  ready :: IO (Setting a)

  start
    :: (All Configurable (Deps a), All (HasConfig env) (Deps a))
    => Setting a
    -> env
    -> IO (Running a)

  activate
    :: (All Configurable (Deps a), All (HasConfig env) (Deps a))
    => env
    -> IO (Setting a, Running a)
  activate env = do
    setting' <- ready
    running' <- start setting' env
    pure (setting', running')


class Configurable a => HasConfig env a where
  setting :: Lens' env (Setting a)
  running :: Lens' env (Running a)


class FetchSetting a where
  fetchSetting :: Text -> a -> IO a
  default fetchSetting :: (Read a) => Text -> a -> IO a
  fetchSetting key def =
    fromMaybe def . (readMay =<<) <$> lookupEnv (unpack key)

instance FetchSetting Int

instance FetchSetting Text where
  fetchSetting key def =
    maybe def pack <$> lookupEnv (unpack key)


type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

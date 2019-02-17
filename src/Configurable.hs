{-# LANGUAGE TypeFamilyDependencies #-}
module Configurable where

import           ClassyPrelude

import           Control.Lens       (Lens')
import           System.Environment (lookupEnv)


class Configurable (a :: *) where
  type Setting a = r | r -> a
  type Running a = r | r -> a

  ready :: IO (Setting a)

  start :: Setting a -> IO (Running a)

  activate :: IO (Setting a, Running a)
  activate = do
    setting' <- ready
    running' <- start setting'
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

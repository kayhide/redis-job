{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Configurable where

import ClassyPrelude

import Control.Lens (Field1 (..), Field2 (..), Lens')
import Data.Extensible ((:*), Comp (..), Forall, Generate, Member (..),
                        Membership, hgenerate, hitchAt, hlookup, htabulateFor,
                        inclusion, piece, vacancy)
import Data.Extensible.Tangle (TangleT, runTangles)
import Data.Proxy (Proxy (..))
import System.Environment (lookupEnv)


class (Generate (Deps a), Forall Configurable (Deps a)) => Configurable (a :: *) where
  type Setting a = r | r -> a
  type Running a = r | r -> a
  type Deps a :: [*]

  ready :: IO (Setting a)

  start
    :: Setting a
    -> ToConfig :* Deps a
    -> IO (Running a)


setting :: forall xs a. (Member xs a) => Lens' (ToConfig :* xs) (Setting a)
setting = piece . _1

running :: forall xs a. (Member xs a) => Lens' (ToConfig :* xs) (Running a)
running = piece . _2


data ToConfig a where
  ToConfig :: Configurable a => Setting a -> Running a -> ToConfig a

instance (s ~ Setting a) => Field1 (ToConfig a) (ToConfig a) s s where
  _1 f (ToConfig setting' running') =
    (\ s' -> ToConfig s' running') <$> f setting'

instance (s ~ Running a) => Field2 (ToConfig a) (ToConfig a) s s where
  _2 f (ToConfig setting' running') =
    (\ r' -> ToConfig setting' r') <$> f running'


class (Forall (Member xs) (Deps a), Configurable a) => Configurable' xs a
instance (Forall (Member xs) (Deps a), Configurable a) => Configurable' xs a

activate :: forall xs. (Forall (Configurable' xs) xs) => IO (ToConfig :* xs)
activate =
  runTangles
  ( htabulateFor
    (Proxy @(Configurable' xs)) $ \_ -> Comp $ activate' inclusion
  ) vacancy

activate'
    :: forall xs a.
       (Configurable a)
    => Membership xs :* Deps a
    -> TangleT ToConfig xs IO (ToConfig a)
activate' xs = do
  env <- hgenerate (\k -> hitchAt $ hlookup k xs)
  lift $ do
    setting' <- ready
    running' <- start setting' env
    pure $ ToConfig setting' running'


class FetchSetting a where
  fetchSetting :: Text -> a -> IO a
  default fetchSetting :: (Read a) => Text -> a -> IO a
  fetchSetting key def =
    fromMaybe def . (readMay =<<) <$> lookupEnv (unpack key)

instance FetchSetting Int

instance FetchSetting Bool where
  fetchSetting key def = do
    let falses = ["false", "f", "0"] :: [Text]
    maybe def ((`notElem` falses) . toLower . pack) <$> lookupEnv (unpack key)

instance FetchSetting Text where
  fetchSetting key def =
    maybe def pack <$> lookupEnv (unpack key)

instance FetchSetting [Text] where
  fetchSetting key def =
    maybe def (words . pack) <$> lookupEnv (unpack key)

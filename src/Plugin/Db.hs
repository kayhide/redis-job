module Plugin.Db where

import ClassyPrelude

import Control.Lens (view)
import Database.Persist.Sql

import Configurable (HasConfig (..))
import Plugin.Db.Config


type Config = DbConfig

run
  :: (HasConfig env DbConfig, MonadReader env m, MonadUnliftIO m)
  => ReaderT SqlBackend m a
  -> m a
run sql = do
  pool' <- view $ running @_ @DbConfig . pool
  runSqlPool sql pool'

module Plugin.Db where

import           ClassyPrelude

import           Control.Lens         ((^.))
import           Database.Persist.Sql

import           Plugin.Db.Config


run
  :: (HasConfig env, MonadReader env m, MonadIO m, MonadUnliftIO m)
  => ReaderT SqlBackend m a
  -> m a
run sql = do
  pool' <- asks (^. running . pool)
  runSqlPool sql pool'

module Plugin.Db where

import           ClassyPrelude

import           Control.Lens         ((^.))
import           Database.Persist.Sql

import           Configurable         (HasConfig (..))
import           Plugin.Db.Config


run
  :: (HasConfig env DbConfig, MonadReader env m, MonadIO m, MonadUnliftIO m)
  => ReaderT SqlBackend m a
  -> m a
run sql = do
  pool' <- asks (^. running @_ @DbConfig . pool)
  runSqlPool sql pool'

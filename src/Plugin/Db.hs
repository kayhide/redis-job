module Plugin.Db where

import ClassyPrelude

import Control.Lens (view)
import Database.Beam.Postgres
import Data.Pool (withResource)

import Configurable (HasConfig (..))
import Plugin.Db.Config


type Config = DbConfig

run
  :: (HasConfig env DbConfig, MonadReader env m, MonadIO m)
  => Pg a
  -> m a
run sql = do
  pool' <- view $ running @_ @DbConfig . pool
  conn' <- liftIO $ withResource pool' pure
  liftIO $ runBeamPostgresDebug (sayErr . pack) conn' sql

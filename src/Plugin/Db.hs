module Plugin.Db where

import ClassyPrelude

import Configurable (ToConfig, running)
import Control.Lens (view)
import Data.Extensible ((:*), Member)
import Database.Persist.Sql
import Plugin.Db.Config


type Config = DbConfig

run
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadUnliftIO m
     )
  => ReaderT SqlBackend m a
  -> m a
run sql = do
  pool' <- view $ running @_ @Config . pool
  runSqlPool sql pool'

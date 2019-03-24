module Plugin.Db where

import ClassyPrelude

import Configurable (ToConfig, running)
import Control.Lens (view)
import Data.Extensible ((:*), Member)
import Database.Persist.Sql
import Database.Selda
import Database.Selda.Backend (runSeldaT)
import Data.Pool (withResource)
import Plugin.Db.Config


type Config = DbConfig

run
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     , MonadMask m
     )
  => SeldaT m a
  -> m a
run sql = do
  pool' <- view $ running @_ @DbConfig . pool
  conn' <- liftIO $ withResource pool' pure
  runSeldaT sql conn'

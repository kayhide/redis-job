module Plugin.Db where

import ClassyPrelude

import Control.Lens (view)
import Database.Selda
import Database.Selda.Backend (runSeldaT)
import Data.Pool (withResource)

import Configurable (HasConfig (..))
import Plugin.Db.Config


type Config = DbConfig

run
  :: (HasConfig env DbConfig, MonadReader env m, MonadIO m, MonadMask m)
  => SeldaT m a
  -> m a
run sql = do
  pool' <- view $ running @_ @DbConfig . pool
  conn' <- liftIO $ withResource pool' pure
  runSeldaT sql conn'

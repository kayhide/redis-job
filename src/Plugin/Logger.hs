module Plugin.Logger where

import           ClassyPrelude

import           Control.Lens         (view)
import           Control.Monad.Logger (LogLevel (..))

import           Configurable         (HasConfig (..))
import           Plugin.Logger.Config


debug
  :: (HasConfig env LoggerConfig, MonadReader env m, MonadIO m)
  => Text
  -> m ()
debug msg' = logOutput LevelDebug msg'

info
  :: (HasConfig env LoggerConfig, MonadReader env m, MonadIO m)
  => Text
  -> m ()
info msg' = logOutput LevelInfo msg'

warn
  :: (HasConfig env LoggerConfig, MonadReader env m, MonadIO m)
  => Text
  -> m ()
warn msg' = logOutput LevelWarn msg'

error
  :: (HasConfig env LoggerConfig, MonadReader env m, MonadIO m)
  => Text
  -> m ()
error msg' = logOutput LevelError msg'

logOutput
  :: (HasConfig env LoggerConfig, MonadReader env m, MonadIO m)
  => LogLevel
  -> Text
  -> m ()
logOutput level' msg' = do
  func' <- view $ running @_ @LoggerConfig . func
  liftIO $ func' level' msg'

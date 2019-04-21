module Plugin.Logger where

import ClassyPrelude

import Configurable (ToConfig, running)
import Control.Lens (view)
import Control.Monad.Logger (LogLevel (..))
import Data.Extensible ((:*), Member)
import Plugin.Logger.Config


type Config = LoggerConfig

debug
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
debug msg' = logOutput LevelDebug msg'

info
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
info msg' = logOutput LevelInfo msg'

warn
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
warn msg' = logOutput LevelWarn msg'

error
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
error msg' = logOutput LevelError msg'

logOutput
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => LogLevel
  -> Text
  -> m ()
logOutput level' msg' = do
  func' <- view $ running @_ @LoggerConfig . func
  liftIO $ func' level' msg'

{-# LANGUAGE TemplateHaskell #-}
module Plugin.Sidekiq.Config where

import           ClassyPrelude

import           Control.Lens    (Lens')
import           Control.Lens.TH (makeFieldsNoPrefix)

import           Configurable    (Configurable (..), fetchSetting)


data SidekiqConfig

data SidekiqSetting = SidekiqSetting
  { _namespace   :: Text
  , _concurrency :: Int
  }
  deriving (Eq, Show)


data SidekiqRunning = SidekiqRunning
  { _workers :: [Async ()]
  , _jobs    :: TChan (IO ())
  }

instance Show SidekiqRunning where
  show _ =
    "SidekiqRunning "
    <> "{_workers = [...]"
    <> ", _jobs = [...]"
    <> "}"

$(makeFieldsNoPrefix ''SidekiqSetting)
$(makeFieldsNoPrefix ''SidekiqRunning)


instance Configurable SidekiqConfig where
  type Setting SidekiqConfig = SidekiqSetting
  type Running SidekiqConfig = SidekiqRunning

  ready =
    SidekiqSetting
    <$> fetchSetting "SIDEKIQ_NAMESPACE" "sidekiq"
    <*> fetchSetting "SIDEKIQ_CONCURRENCY" 5

  start (SidekiqSetting _ concurrency') = do
    jobs' <- atomically newTChan
    workers' <- replicateM concurrency' . async $ go jobs'
    pure $ SidekiqRunning workers' jobs'
    where
      go :: TChan (IO ()) -> IO ()
      go jobs' = forever $
        join $ atomically $ readTChan jobs'


class HasConfig env where
  setting :: Lens' env SidekiqSetting
  running :: Lens' env SidekiqRunning

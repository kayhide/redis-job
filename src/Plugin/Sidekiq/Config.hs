{-# LANGUAGE TemplateHaskell #-}
module Plugin.Sidekiq.Config where

import           ClassyPrelude

import           Control.Lens        ((^.))
import           Control.Lens.TH     (makeFieldsNoPrefix)
import           Data.Proxy

import           Configurable        (Configurable (..), HasConfig (..),
                                      fetchSetting)
import           Plugin.Redis.Config (RedisConfig, info)


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
  type Deps SidekiqConfig = '[RedisConfig]

  ready _ =
    SidekiqSetting
    <$> fetchSetting "SIDEKIQ_NAMESPACE" "sidekiq"
    <*> fetchSetting "SIDEKIQ_CONCURRENCY" 5

  start _ (SidekiqSetting _ concurrency') env = do
    print $ env ^. running (Proxy @RedisConfig) . info
    jobs' <- atomically newTChan
    workers' <- replicateM concurrency' . async $ go jobs'
    pure $ SidekiqRunning workers' jobs'
    where
      go :: TChan (IO ()) -> IO ()
      go jobs' = forever $
        join $ atomically $ readTChan jobs'

{-# LANGUAGE TemplateHaskell #-}
module Plugin.Sidekiq.Config where

import ClassyPrelude

import Control.Lens (view, (^.))
import Control.Lens.TH (makeFieldsNoPrefix)

import Configurable (Configurable (..), HasConfig (..), fetchSetting)
import qualified Plugin.Logger as Logger
import qualified Plugin.Redis as Redis
import Plugin.Sidekiq.Job


data SidekiqConfig

data SidekiqSetting = SidekiqSetting
  { _namespace   :: Text
  , _concurrency :: Int
  , _queues      :: [Text]
  }
  deriving (Eq, Show)


data SidekiqRunning = SidekiqRunning
  { _workers :: [Async ()]
  , _jobs    :: TChan SomeJob
  , _channel :: TChan (Text, ByteString)
  }

instance Show SidekiqRunning where
  show _ =
    "SidekiqRunning "
    <> "{_workers = [...]"
    <> ", _jobs = [...]"
    <> ", _channel = [...]"
    <> "}"

$(makeFieldsNoPrefix ''SidekiqSetting)
$(makeFieldsNoPrefix ''SidekiqRunning)


instance Configurable SidekiqConfig where
  type Setting SidekiqConfig = SidekiqSetting
  type Running SidekiqConfig = SidekiqRunning
  type Deps SidekiqConfig = '[Logger.Config, Redis.Config]

  ready =
    SidekiqSetting
    <$> fetchSetting "SIDEKIQ_NAMESPACE" "sidekiq"
    <*> fetchSetting "SIDEKIQ_CONCURRENCY" 5
    <*> fetchSetting "SIDEKIQ_QUEUES" ["default"]

  start setting'@(SidekiqSetting _ concurrency' _) env = do
    jobs' <- atomically newTChan
    workers' <- replicateM concurrency' . async $ go jobs'
    channel' <- atomically newTChan
    void $ async $ runReaderT (watch' setting' channel') env
    pure $ SidekiqRunning workers' jobs' channel'
    where
      go :: TChan SomeJob -> IO ()
      go jobs' = forever $ do
        job' <- atomically $ readTChan jobs'
        performNow job'

watch'
  :: ( HasConfig env Logger.Config
     , HasConfig env Redis.Config
     , MonadReader env m
     , MonadIO m
     )
  => SidekiqSetting
  -> TChan (Text, ByteString)
  -> m a
watch' setting' chan'= do
  let queues' = getQueues setting'
  Logger.info $ "Listening to: " <> unwords queues'
  forever $ do
    Redis.run $
      Redis.brpop (encodeUtf8 <$> queues') 1 >>= \case
        Left _   -> fail "Failed to read queue"
        Right x' -> liftIO $ traverse_ post' x'

  where
    post' :: (ByteString, ByteString) -> IO ()
    post' x = do
      atomically $ writeTChan chan' $ first decodeUtf8 x

getQueues :: SidekiqSetting -> [Text]
getQueues setting' = ((namespace' <> ":queue:") <>) <$> queues'
  where
  namespace' = setting' ^. namespace
  queues' = setting' ^. queues

askQueues :: (HasConfig env SidekiqConfig, MonadReader env m, MonadIO m) => m [Text]
askQueues = do
  getQueues <$> view (setting @_ @SidekiqConfig)

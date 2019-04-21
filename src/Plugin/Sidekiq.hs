module Plugin.Sidekiq
  ( Config
  , watch
  , askQueues
  , module Plugin.Sidekiq.Job
  )
where

import ClassyPrelude

import Configurable
import Control.Lens (view)
import Data.Extensible
import Plugin.Sidekiq.Config
import Plugin.Sidekiq.Job


type Config = SidekiqConfig

watch
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => (ByteString -> Maybe SomeJob)
  -> m a
watch decode = do
  chan' <- view $ running @_ @Config . channel
  jobs' <- view $ running @_ @Config . jobs
  liftIO $ do
    forever $ do
      x <- atomically $ readTChan chan'
      post' jobs' x

  where
    post' :: TChan SomeJob -> (Text, ByteString) -> IO ()
    post' jobs' (queue'', x') = do
      putStrLn $ "Fetched from queue: " <> queue''
      case decode x' of
        Nothing  -> fail "Cannot decode"
        Just job -> atomically $ writeTChan jobs' job


askQueues
  :: ( Member xs Config
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => m [Text]
askQueues = do
  getQueues <$> view (setting @_ @Config)

module Plugin.Sidekiq
  ( watch
  , askQueues
  , module Plugin.Sidekiq.Job
  )
where

import ClassyPrelude

import Control.Lens (view)

import Configurable (HasConfig (..))
import Plugin.Sidekiq.Config
import Plugin.Sidekiq.Job

watch
  :: ( HasConfig env SidekiqConfig
     , MonadReader env m
     , MonadIO m
     )
  => (ByteString -> Maybe SomeJob)
  -> m a
watch decode = do
  chan' <- view $ running @_ @SidekiqConfig . channel
  jobs' <- view $ running @_ @SidekiqConfig . jobs
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

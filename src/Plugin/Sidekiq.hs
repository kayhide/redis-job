module Plugin.Sidekiq
  ( watch
  , queue
  , performNow
  , module Plugin.Sidekiq.Job
  )
where

import           ClassyPrelude

import           Control.Lens          ((^.))

import           Configurable          (HasConfig (..))
import           Data.Proxy
import qualified Plugin.Redis          as Redis
import           Plugin.Redis.Config   (RedisConfig)
import           Plugin.Sidekiq.Config
import           Plugin.Sidekiq.Job


watch
  :: (HasConfig env SidekiqConfig, HasConfig env RedisConfig, MonadReader env m, MonadIO m)
  => (Text -> ByteString -> IO ())
  -> m a
watch action = do
  namespace' <- asks (^. setting (Proxy @SidekiqConfig) . namespace)
  let queue' = namespace' <> ":queue:default"
  jobs' <- asks (^. running (Proxy @SidekiqConfig) . jobs)
  putStrLn "Listening to: "
  putStrLn $ "  " <> queue'
  forever $ do
    x <- Redis.run $
      Redis.brpop [encodeUtf8 queue'] 1 >>= \case
        Left _ -> fail "Failed to read queue"
        Right x' -> pure x'
    traverse_ (liftIO . post' jobs') x

  where
    post' :: TChan (IO ()) -> (ByteString, ByteString) -> IO ()
    post' jobs' (queue'', x') =
      atomically $ writeTChan jobs' $ action (decodeUtf8 queue'') x'

queue :: (HasConfig env SidekiqConfig, MonadReader env m, MonadIO m) => m Text
queue = (<> ":queue:default") <$> asks (^. setting (Proxy @SidekiqConfig) . namespace)

performNow :: (Job a, Args a ~ args) => JobWrapper a -> IO ()
performNow wrapper =
  traverse_ perform $ do
  args' <- headMay $ _args wrapper
  headMay $ _arguments args'

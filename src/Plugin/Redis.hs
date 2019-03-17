module Plugin.Redis where

import ClassyPrelude

import Control.Lens ((^.))
import Database.Redis as Redis

import Configurable (HasConfig (..))
import Plugin.Redis.Config


run
  :: (HasConfig env RedisConfig, MonadReader env m, MonadIO m)
  => Redis.Redis a
  -> m a
run redis = do
  conn' <- asks (^. running @_ @RedisConfig . conn)
  liftIO $ Redis.runRedis conn' redis

brpop :: (RedisCtx m f)
      => [ByteString] -> Integer -> m (f (Maybe (ByteString, ByteString)))
brpop = Redis.brpop
